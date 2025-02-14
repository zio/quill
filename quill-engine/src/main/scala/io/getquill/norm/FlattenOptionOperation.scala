package io.getquill.norm

import io.getquill.{HasStatelessCache, StatelessCache}
import io.getquill.ast._
import io.getquill.ast.Implicits._
import io.getquill.norm.ConcatBehavior.NonAnsiConcat
import io.getquill.quat.QuatOps.HasBooleanQuat
import io.getquill.util.Messages.TraceType
import io.getquill.util.{Interpolator, TraceConfig}

class FlattenOptionOperation(val cache: StatelessCache, concatBehavior: ConcatBehavior, traceConfig: TraceConfig) extends StatelessTransformer with HasStatelessCache {

  val interp = new Interpolator(TraceType.FlattenOptionOperation, traceConfig, 2)
  import interp._

  private def emptyOrNot(b: Boolean, ast: Ast) =
    if (b) OptionIsEmpty(ast) else OptionNonEmpty(ast)

  def validateContainsOrElse(containsNon: Boolean, succeedWith: () => Ast, orElse: () => Ast) =
    if (containsNon) succeedWith()
    else orElse()

  def uncheckedReduction(ast: Ast, alias: Ident, body: Ast, validateBody: Ast => Boolean) =
    validateContainsOrElse(
      validateBody(body),
      () => {
        val reduced = BetaReduction(body, alias -> ast)
        apply(IfExistElseNull(ast, reduced))
      },
      () => apply(BetaReduction(body, alias -> ast))
    )

  def uncheckedForall(ast: Ast, alias: Ident, body: Ast, validateBody: Ast => Boolean) =
    validateContainsOrElse(
      validateBody(body),
      () => {
        val reduction = BetaReduction(body, alias -> ast)
        apply(((reduction +&&+ IsNotNullCheck(ast)) +||+ IsNullCheck(ast)): Ast)
      },
      () => {
        val reduced = BetaReduction(body, alias -> ast)
        apply((reduced +||+ IsNullCheck(ast)): Ast)
      }
    )

  def containsNonFallthroughElement(ast: Ast) =
    CollectAst(ast) {
      case If(_, _, _)                                                                    => true
      case Infix(_, _, _, _, _)                                                           => true
      case BinaryOperation(_, StringOperator.`+`, _) if (concatBehavior == NonAnsiConcat) => true
    }.nonEmpty

  override def apply(ast: Ast): Ast = cached(ast) {
    trace"Flattening option clause $ast ".andReturnIf {
      ast match {

        case OptionTableFlatMap(ast, alias, body) =>
          uncheckedReduction(ast, alias, body, _ => false)

        case OptionTableMap(ast, alias, body) =>
          uncheckedReduction(ast, alias, body, _ => false)

        case OptionTableExists(ast, alias, body) =>
          uncheckedReduction(ast, alias, body, _ => false)

        case OptionTableForall(ast, alias, body) =>
          uncheckedForall(ast, alias, body, _ => false)

        case OptionFlatten(ast) =>
          apply(ast)

        case OptionSome(ast) =>
          apply(ast)

        case OptionApply(ast) =>
          apply(ast)

        case OptionOrNull(ast) =>
          apply(ast)

        case OptionGetOrNull(ast) =>
          apply(ast)

        case OptionGetOrElse(HasBooleanQuat(OptionMap(ast, alias, body)), HasBooleanQuat(alternative)) =>
          val expr        = BetaReduction(body, alias -> ast)
          val output: Ast = (expr +&&+ IsNotNullCheck(ast)) +||+ (alternative +&&+ IsNullCheck(ast))
          apply(output)

        case OptionGetOrElse(ast, body) =>
          apply(If(IsNotNullCheck(ast), ast, body))

        case OptionOrElse(ast, body) =>
          apply(If(IsNotNullCheck(ast), ast, body))

        case OptionFlatMap(ast, alias, body) =>
          uncheckedReduction(ast, alias, body, containsNonFallthroughElement)

        case OptionMap(ast, alias, body) =>
          uncheckedReduction(ast, alias, body, containsNonFallthroughElement)

        // a.orElse(b).forAll(alias => body) becomes:
        //    body(->a) || a==null && body(->b) || a==null && b==null
        //
        // Note that since all of the clauses are boolean this a.orElse(...) can be replaced
        // by a||(b && a==null). If the clause was actually returning value (e.g. if(a) foo else bar)
        // then these kinds of reductions would not be possible.
        // Leaving the ||a==null clause without reversing for now despite the fact that ==null
        // clauses shuold generally be the 2nd in the order because of the <> issue.
        case OptionForall(OptionOrElse(a, b), alias, body) =>
          val reducedA = BetaReduction(body, alias -> a)
          val reducedB = BetaReduction(body, alias -> b)
          val isNullA  = IsNullCheck(a)
          val isNullB  = IsNullCheck(b)

          apply(reducedA) +||+ apply((isNullA +&&+ reducedB): Ast) +||+ apply((isNullA +&&+ isNullB): Ast)

        case OptionForall(ast, alias, body) =>
          uncheckedForall(ast, alias, body, containsNonFallthroughElement)

        case OptionExists(OptionOrElse(a, b), alias, body) =>
          val reducedA = BetaReduction(body, alias -> a)
          val reducedB = BetaReduction(body, alias -> b)
          apply((reducedA +&&+ IsNotNullCheck(a)): Ast) +||+ apply((reducedB +&&+ IsNotNullCheck(b)): Ast)

        case OptionExists(ast, alias, body) =>
          validateContainsOrElse(
            containsNonFallthroughElement(body),
            () => {
              val reduction = BetaReduction(body, alias -> ast)
              apply(reduction +&&+ IsNotNullCheck(ast): Ast)
            },
            () => apply(BetaReduction(body, alias -> ast))
          )

        case OptionContains(ast, body) =>
          apply((ast +==+ body): Ast)

        case FilterIfDefined(ast, alias, body) =>
          uncheckedForall(ast, alias, body, containsNonFallthroughElement)

        case other =>
          super.apply(other)
      }
    }(_ != ast)
  }
}
