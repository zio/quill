package io.getquill.norm.capture

import io.getquill.StatefulCache
import io.getquill.ast._
import io.getquill.norm.BetaReduction
import io.getquill.util.{Interpolator, TraceConfig}
import io.getquill.util.Messages.TraceType

case class Dealias(state: Option[Ident], traceConfig: TraceConfig, override val cache: StatefulCache[Option[Ident]]) extends StatefulTransformer[Option[Ident]] {
  def Dealias(state: Option[Ident]) = new Dealias(state, traceConfig, cache)

  val interp = new Interpolator(TraceType.Standard, traceConfig, 3)
  import interp._

  override def apply(q: Query): (Query, StatefulTransformer[Option[Ident]]) =
    q match {
      case FlatMap(a, b, c) =>
        dealias(a, b, c)(FlatMap) match {
          case (FlatMap(a, b, c), _) =>
            val (cn, cnt) = apply(c)
            (FlatMap(a, b, cn), cnt)
        }
      case ConcatMap(a, b, c) =>
        dealias(a, b, c)(ConcatMap) match {
          case (ConcatMap(a, b, c), _) =>
            val (cn, cnt) = apply(c)
            (ConcatMap(a, b, cn), cnt)
        }
      case Map(a, b, c) =>
        dealias(a, b, c)(Map)
      case Filter(a, b, c) =>
        dealias(a, b, c)(Filter)
      case SortBy(a, b, c, d) =>
        dealias(a, b, c)(SortBy(_, _, _, d))
      case GroupBy(a, b, c) =>
        dealias(a, b, c)(GroupBy)
      case g @ GroupByMap(qry, b, c, d, e) =>
        apply(qry) match {
          case (an, t @ Dealias(Some(alias), _, _)) =>
            val b1 = alias.copy(quat = b.quat)
            val d1 = alias.copy(quat = d.quat)
            (GroupByMap(an, b1, BetaReduction(c, b -> b1), d1, BetaReduction(e, d -> d1)), t)
          case other =>
            (g, Dealias(Some(b)))
        }
      case DistinctOn(a, b, c) =>
        dealias(a, b, c)(DistinctOn)
      case Take(a, b) =>
        val (an, ant) = apply(a)
        (Take(an, b), ant)
      case Drop(a, b) =>
        val (an, ant) = apply(a)
        (Drop(an, b), ant)
      case Union(a, b) =>
        val (an, _) = apply(a)
        val (bn, _) = apply(b)
        (Union(an, bn), Dealias(None))
      case UnionAll(a, b) =>
        val (an, _) = apply(a)
        val (bn, _) = apply(b)
        (UnionAll(an, bn), Dealias(None))
      case Join(t, a, b, iA, iB, o) =>
        val ((an, iAn, on), _)  = dealias(a, iA, o)((_, _, _))
        val ((bn, iBn, onn), _) = dealias(b, iB, on)((_, _, _))
        (Join(t, an, bn, iAn, iBn, onn), Dealias(None))
      case FlatJoin(t, a, iA, o) =>
        val ((an, iAn, on), ont) = dealias(a, iA, o)((_, _, _))
        (FlatJoin(t, an, iAn, on), Dealias(Some(iA)))
      case _: Entity | _: Distinct | _: Aggregation | _: Nested =>
        (q, Dealias(None))
    }

  private def dealias[T](a: Ast, b: Ident, c: Ast)(f: (Ast, Ident, Ast) => T) =
    apply(a) match {
      case (an, t @ Dealias(Some(alias), _, _)) =>
        val retypedAlias = alias.copy(quat = b.quat)
        trace"Dealias $b into $retypedAlias".andLog()
        (f(an, retypedAlias, BetaReduction(c, b -> retypedAlias)), t)
      case other =>
        (f(a, b, c), Dealias(Some(b)))
    }
}

object Dealias {
  def apply(query: Query)(traceConfig: TraceConfig, cache: StatefulCache[Option[Ident]]) =
    new Dealias(None, traceConfig, cache)(query) match {
      case (q, _) => q
    }
}

class DealiasApply(traceConfig: TraceConfig, cache: StatefulCache[Option[Ident]]) {
  def apply(query: Query) =
    new Dealias(None, traceConfig, cache)(query) match {
      case (q, _) => q
    }
}
