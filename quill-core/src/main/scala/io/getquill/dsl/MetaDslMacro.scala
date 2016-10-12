package io.getquill.dsl

import io.getquill.util.Messages._
import scala.reflect.macros.whitebox.{ Context => MacroContext }
import io.getquill.util.OptionalTypecheck

class MetaDslMacro(val c: MacroContext) {
  import c.universe._

  def schemaMeta[T](entity: Tree, columns: Tree*)(implicit t: WeakTypeTag[T]): Tree =
    c.untypecheck {
      q"""
        new ${c.prefix}.SchemaMeta[$t] {
          override val entity =
            ${c.prefix}.quote {
              ${c.prefix}.querySchema[$t]($entity, ..$columns)
            }
        }
      """
    }

  def queryMeta[T, R](expand: Tree)(extract: Tree)(implicit t: WeakTypeTag[T], r: WeakTypeTag[R]): Tree =
    c.untypecheck {
      q"""
        new ${c.prefix}.QueryMeta[$t] {
          override val expand = $expand
          override val extract =
            (r: ${c.prefix}.ResultRow) => $extract(implicitly[${c.prefix}.QueryMeta[$r]].extract(r))
        }
      """
    }

  def insertMeta[T](exclude: Tree*)(implicit t: WeakTypeTag[T]): Tree =
    actionMeta[T](value("Encoder", t.tpe, exclude: _*), "insert")

  def updateMeta[T](exclude: Tree*)(implicit t: WeakTypeTag[T]): Tree =
    actionMeta[T](value("Encoder", t.tpe, exclude: _*), "update")

  def materializeQueryMeta[T](implicit t: WeakTypeTag[T]): Tree = {
    val value = this.value("Decoder", t.tpe)
    q"""
      new ${c.prefix}.QueryMeta[$t] {
        override val expand = ${expandQuery[T](value)}
        override val extract = ${extract[T](value)}
      }
    """
  }

  def materializeUpdateMeta[T](implicit t: WeakTypeTag[T]): Tree =
    actionMeta[T](value("Encoder", t.tpe), "update")

  def materializeInsertMeta[T](implicit t: WeakTypeTag[T]): Tree =
    actionMeta[T](value("Encoder", t.tpe), "insert")

  def materializeSchemaMeta[T](implicit t: WeakTypeTag[T]): Tree =
    (t.tpe.typeSymbol.isClass && t.tpe.typeSymbol.asClass.isCaseClass) match {
      case true =>
        q"""
          new ${c.prefix}.SchemaMeta[$t] {
            override val entity =
              ${c.prefix}.quote(${c.prefix}.querySchema[$t](${t.tpe.typeSymbol.name.decodedName.toString}))
          }
        """
      case false =>
        c.fail(s"Can't materialize a `SchemaMeta` for non-case-class type '${t.tpe}', please provide an implicit `SchemaMeta`.")
    }

  private def expandQuery[T](value: Value)(implicit t: WeakTypeTag[T]) = {
    val elements = flatten(q"x", value)
    q"${c.prefix}.quote((q: ${c.prefix}.Query[$t]) => q.map(x => io.getquill.dsl.UnlimitedTuple(..$elements)))"
  }

  private def extract[T](value: Value)(implicit t: WeakTypeTag[T]): Tree = {
    var index = -1
    def expand(value: Value, optional: Boolean = false): Tree =
      value match {

        case Scalar(path, tpe, decoder) =>
          index += 1
          optional match {
            case true =>
              q"implicitly[${c.prefix}.Decoder[Option[$tpe]]].apply($index, row)"
            case false =>
              q"$decoder($index, row)"
          }

        case Nested(term, tpe, params) =>
          q"new $tpe(...${params.map(_.map(expand(_)))})"

        case OptionalNested(term, tpe, params) =>
          val groups = params.map(_.map(expand(_, optional = true)))
          val terms =
            groups.zipWithIndex.map {
              case (options, idx1) =>
                (0 until options.size).map { idx2 =>
                  TermName(s"o_${idx1}_${idx2}")
                }
            }
          groups.zipWithIndex.foldLeft(q"Some(new $tpe(...$terms))") {
            case (body, (options, idx1)) =>
              options.zipWithIndex.foldLeft(body) {
                case (body, (option, idx2)) =>
                  val o = q"val ${TermName(s"o_${idx1}_${idx2}")} = $EmptyTree"
                  q"$option.flatMap($o => $body)"
              }
          }
      }
    q"(row: ${c.prefix}.ResultRow) => ${expand(value)}"
  }

  private def actionMeta[T](value: Value, method: String)(implicit t: WeakTypeTag[T]) = {
    val assignments =
      flatten(q"v", value)
        .zip(flatten(q"value", value))
        .map {
          case (vTree, valueTree) =>
            q"(v: $t) => $vTree -> $valueTree"
        }
    c.untypecheck {
      q"""
        new ${c.prefix}.${TypeName(method.capitalize + "Meta")}[$t] {
          override val expand =
            ${c.prefix}.quote((q: ${c.prefix}.EntityQuery[$t], value: $t) => q.${TermName(method)}(..$assignments))
        }
      """
    }
  }

  def flatten(base: Tree, value: Value): List[Tree] = {
    def nest(tree: Tree, term: Option[TermName]) =
      term match {
        case None       => tree
        case Some(term) => q"$tree.$term"
      }
    def apply(base: Tree, params: List[List[Value]]): List[Tree] =
      params.flatten.map(flatten(base, _)).flatten
    value match {
      case Scalar(term, tpe, decoder) =>
        List(nest(base, term))
      case Nested(term, tpe, params) =>
        apply(nest(base, term), params)
      case OptionalNested(term, tpe, params) =>
        apply(q"v", params)
          .map(body => q"${nest(base, term)}.map(v => $body)")
    }
  }

  sealed trait Value {
    val term: Option[TermName]
  }
  case class Nested(term: Option[TermName], tpe: Type, params: List[List[Value]]) extends Value
  case class OptionalNested(term: Option[TermName], tpe: Type, params: List[List[Value]]) extends Value
  case class Scalar(term: Option[TermName], tpe: Type, decoder: Tree) extends Value

  private def is[T](tpe: Type)(implicit t: TypeTag[T]) =
    tpe <:< t.tpe

  private def value(encoding: String, tpe: Type, exclude: Tree*): Value = {

    def nest(tpe: Type, term: Option[TermName]): Nested =
      caseClassConstructor(tpe) match {
        case None =>
          c.fail(s"Found the embedded '$tpe', but it is not a case class")
        case Some(constructor) =>
          val params =
            constructor.paramLists.map {
              _.map { param =>
                apply(
                  param.typeSignature.asSeenFrom(tpe, tpe.typeSymbol),
                  Some(param.name.toTermName),
                  nested = !isTuple(tpe)
                )
              }
            }
          Nested(term, tpe, params)
      }

    def apply(tpe: Type, term: Option[TermName], nested: Boolean): Value = {
      OptionalTypecheck(c)(q"implicitly[${c.prefix}.${TypeName(encoding)}[$tpe]]") match {
        case Some(encoding) => Scalar(term, tpe, encoding)
        case None =>

          def value(tpe: Type) =
            tpe match {
              case tpe if (!is[MetaDsl#Embedded](tpe) && nested) =>
                c.fail(
                  s"Can't expand nested value '$tpe', please make it an `Embedded` " +
                    s"case class or provide an implicit $encoding for it."
                )

              case tpe =>
                nest(tpe, term)
            }

          is[Option[Any]](tpe) match {
            case false =>
              value(tpe)
            case true =>
              val nested = value(tpe.typeArgs.head)
              OptionalNested(nested.term, nested.tpe, nested.params)
          }
      }
    }

    def filterExcludes(value: Value) = {
      val paths =
        exclude.map {
          case f: Function =>
            def path(tree: Tree): List[TermName] =
              tree match {
                case q"$a.$b"                => path(a) :+ b
                case q"$a.map[$t]($b => $c)" => path(a) ++ path(c)
                case other                   => Nil
              }
            path(f.body)
        }

      def filter(value: Value, path: List[TermName] = Nil): Option[Value] =
        value match {
          case value if (paths.contains(path ++ value.term)) =>
            None
          case Nested(term, tpe, params) =>
            Some(Nested(term, tpe, params.map(_.map(filter(_, path ++ term)).flatten)))
          case OptionalNested(term, tpe, params) =>
            Some(OptionalNested(term, tpe, params.map(_.map(filter(_, path ++ term)).flatten)))
          case value =>
            Some(value)
        }

      filter(value).getOrElse {
        c.fail("Can't exclude all entity properties")
      }
    }

    filterExcludes(apply(tpe, term = None, nested = false))
  }

  private def isTuple(tpe: Type) =
    tpe.typeSymbol.name.toString.startsWith("Tuple")

  private def caseClassConstructor(t: Type) =
    t.members.collect {
      case m: MethodSymbol if (m.isPrimaryConstructor) => m
    }.headOption
}