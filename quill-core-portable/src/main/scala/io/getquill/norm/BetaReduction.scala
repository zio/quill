package io.getquill.norm

import io.getquill.ast._

import scala.collection.immutable.{ Map => IMap }

/**
 * How do we want beta reduction to treat Quats? Typically the right answer is when any variable x type X
 * is reduced to t type T we check that T is a subtype of X and replace it e.g:
 * <pre>x.foo reduce v:V -> t:T where V is CC(foo:V) and T is CC(foo:V, bar:V)</pre>
 * (NOTE: see the notes on Quat Shorthand Syntax in Quats.scala if unfamiliar with the syntax above)
 * However if T is not a subtype of X, then we need to throw an error. The exception to this is
 * in the case where we are substutiting a real type for a Quat.Null or Quat.Generic (roughly speaking, a 'Bottom Type').
 * In that case, just do the substitution.
 * This general behavior we call `SubstituteSubtypes`, it is also considered the default.
 *
 * The behavior with variable-renaming in `PropagateRenames` is and `ReifyLiftings` slightly different.
 * In these cases, it is a carte-blanche replacement of properties that is necessary. In this case
 * we are either plugging in a Generic type that is being specialized (e.g. X is Quat.Generic) or
 * reducing some type CC(foo:V) to the corresponding renamed type CC(foo:V)[foo->renameFoo].
 * This general behavior we call `ReplaceWithReduction` i.e. Quat types are replaced with whatever
 * varaibles are being beta-reduced irregardless of subtyping.
 */
sealed trait TypeBehavior
object TypeBehavior {
  case object SubstituteSubtypes extends TypeBehavior
  case object ReplaceWithReduction extends TypeBehavior
}

case class BetaReduction(map: IMap[Ast, Ast], typeBehavior: TypeBehavior)
  extends StatelessTransformer {

  override def apply(ast: Ast): Ast =
    ast match {

      case ast if map.contains(ast) =>
        val rep = BetaReduction(map - ast - map(ast), typeBehavior)(map(ast))
        val output =
          rep match {
            // If we are ignoring types, just do the replacement. Need to do this if we are doing renames since a
            // carte-blanche replace is required. E.g. replace CC(a,b) with CC(a,b)[a->foo] will never happen because
            // CC(a,b)[a->foo] is a subtype of CC(a,b)
            case _ if (typeBehavior == TypeBehavior.ReplaceWithReduction) =>
              rep

            // In this case the terminal must be a subtype of this type
            case BottomTypedTerminal(terminal) =>
              terminal.withQuat(ast.quat)

            case Terminal(terminal) =>
              val leastUpperType = terminal.quat.leastUpperType(ast.quat)
              if (leastUpperType.isDefined)
                terminal.withQuat(leastUpperType.head)
              else
                throw new IllegalArgumentException(s"Cannot beta reduce [$rep <- $ast] because ${rep.quat.shortString} [of:${rep}] is not a subtype of ${ast.quat.shortString} [of:${ast}]")

            case other =>
              other
          }

        output

      case Property(Tuple(values), name) =>
        apply(values(name.drop(1).toInt - 1))

      case Property(CaseClass(tuples), name) =>
        apply(tuples.toMap.apply(name))

      case FunctionApply(Function(params, body), values) =>
        val conflicts = values.flatMap(CollectAst.byType[Ident]).map { (i: Ident) =>
          i -> Ident(s"tmp_${i.name}", i.quat)
        }.toMap[Ident, Ident]
        val newParams = params.map { p =>
          conflicts.getOrElse(p, p)
        }
        // Prepending IMap[Ast, Ast]() is needed otherwise 2.13 fails complaining:
        // Note: io.getquill.ast.Ident <: io.getquill.ast.Ast, but trait Map is invariant in type K.
        val bodyr = new BetaReduction(IMap[Ast, Ast]() ++ conflicts ++ params.zip(newParams), typeBehavior).apply(body)
        apply(BetaReduction(map ++ newParams.zip(values).toMap, typeBehavior).apply(bodyr))

      case Function(params, body) =>
        val newParams = params.map { p =>
          map.get(p) match {
            case Some(i: Ident) => i
            case _              => p
          }
        }
        Function(newParams, BetaReduction(map ++ params.zip(newParams), typeBehavior)(body))

      case Block(statements) =>
        apply {
          // Walk through the statements, last to the first
          statements.reverse.tail.foldLeft((IMap[Ast, Ast](), statements.last)) {
            case ((map, stmt), line) =>
              // Beta-reduce the statements from the end to the beginning
              BetaReduction(map, typeBehavior)(line) match {
                // If the beta reduction is a some 'val x=t', add x->t to the beta reductions map
                case Val(name, body) =>
                  val newMap = map + (name -> body)
                  val newStmt = BetaReduction(stmt, newMap, typeBehavior)
                  (newMap, newStmt)
                case _ =>
                  (map, stmt)
              }
          }._2
        }

      case Foreach(query, alias, body) =>
        Foreach(query, alias, BetaReduction(map - alias, typeBehavior)(body))
      case Returning(action, alias, prop) =>
        val t = BetaReduction(map - alias, typeBehavior)
        Returning(apply(action), alias, t(prop))

      case ReturningGenerated(action, alias, prop) =>
        val t = BetaReduction(map - alias, typeBehavior)
        ReturningGenerated(apply(action), alias, t(prop))

      case other =>
        super.apply(other)
    }

  override def apply(o: OptionOperation): OptionOperation =
    o match {
      case other @ OptionTableFlatMap(a, b, c) =>
        OptionTableFlatMap(apply(a), b, BetaReduction(map - b, typeBehavior)(c))
      case OptionTableMap(a, b, c) =>
        OptionTableMap(apply(a), b, BetaReduction(map - b, typeBehavior)(c))
      case OptionTableExists(a, b, c) =>
        OptionTableExists(apply(a), b, BetaReduction(map - b, typeBehavior)(c))
      case OptionTableForall(a, b, c) =>
        OptionTableForall(apply(a), b, BetaReduction(map - b, typeBehavior)(c))
      case other @ OptionFlatMap(a, b, c) =>
        OptionFlatMap(apply(a), b, BetaReduction(map - b, typeBehavior)(c))
      case OptionMap(a, b, c) =>
        OptionMap(apply(a), b, BetaReduction(map - b, typeBehavior)(c))
      case OptionForall(a, b, c) =>
        OptionForall(apply(a), b, BetaReduction(map - b, typeBehavior)(c))
      case OptionExists(a, b, c) =>
        OptionExists(apply(a), b, BetaReduction(map - b, typeBehavior)(c))
      case other =>
        super.apply(other)
    }

  override def apply(e: Assignment): Assignment =
    e match {
      case Assignment(alias, prop, value) =>
        val t = BetaReduction(map - alias, typeBehavior)
        Assignment(alias, t(prop), t(value))
    }

  override def apply(query: Query): Query =
    query match {
      case Filter(a, b, c) =>
        Filter(apply(a), b, BetaReduction(map - b, typeBehavior)(c))
      case Map(a, b, c) =>
        Map(apply(a), b, BetaReduction(map - b, typeBehavior)(c))
      case FlatMap(a, b, c) =>
        FlatMap(apply(a), b, BetaReduction(map - b, typeBehavior)(c))
      case ConcatMap(a, b, c) =>
        ConcatMap(apply(a), b, BetaReduction(map - b, typeBehavior)(c))
      case SortBy(a, b, c, d) =>
        SortBy(apply(a), b, BetaReduction(map - b, typeBehavior)(c), d)
      case GroupBy(a, b, c) =>
        GroupBy(apply(a), b, BetaReduction(map - b, typeBehavior)(c))
      case Join(t, a, b, iA, iB, on) =>
        Join(t, apply(a), apply(b), iA, iB, BetaReduction(map - iA - iB, typeBehavior)(on))
      case FlatJoin(t, a, iA, on) =>
        FlatJoin(t, apply(a), iA, BetaReduction(map - iA, typeBehavior)(on))
      case _: Take | _: Entity | _: Drop | _: Union | _: UnionAll | _: Aggregation | _: Distinct | _: Nested =>
        super.apply(query)
    }
}

object BetaReduction {

  private def checkQuats(body: Ast, replacements: Seq[(Ast, Ast)]) =
    replacements.foreach {
      case (orig, rep) =>
        //if (orig.quat != rep.quat)
        if (rep.quat.leastUpperType(orig.quat).isEmpty)
          throw new IllegalArgumentException(s"Cannot beta reduce [$rep <- $orig] within [$body] because ${rep.quat.shortString} [of:${rep}] is not a subtype of ${orig.quat.shortString} [of:${orig}]")
    }

  def apply(ast: Ast, typeBehavior: TypeBehavior, t: (Ast, Ast)*): Ast = {
    typeBehavior match {
      case TypeBehavior.SubstituteSubtypes =>
        checkQuats(ast, t)
      case _ =>
    }
    val output = apply(ast, t.toMap, typeBehavior)
    output
  }

  def apply(ast: Ast, t: (Ast, Ast)*): Ast =
    apply(ast, TypeBehavior.SubstituteSubtypes, t: _*)

  def apply(ast: Ast, replacements: IMap[Ast, Ast], typeBehavior: TypeBehavior): Ast = {
    typeBehavior match {
      case TypeBehavior.SubstituteSubtypes =>
        checkQuats(ast, replacements.toSeq)
      case _ =>
    }
    BetaReduction(replacements, typeBehavior).apply(ast) match {
      // Since it is possible for the AST to match but the match not be exactly the same (e.g.
      // if a AST property not in the product cases comes up (e.g. Ident's quat.rename etc...) make
      // sure to return the actual AST that was matched as opposed to the one passed in.
      case matchingAst @ `ast` => matchingAst
      case other               => apply(other, IMap[Ast, Ast](), typeBehavior)
    }
  }
}
