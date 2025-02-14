package io.getquill.norm

import io.getquill.{HasStatelessCacheOpt, StatelessCacheOpt}
import io.getquill.ast._

class NormalizeNestedStructures(normalize: Normalize, val cache: StatelessCacheOpt) extends HasStatelessCacheOpt {

  def unapply(q: Query): Option[Query] = cached(q) {
    q match {
      case e: Entity          => None
      case Map(a, b, c)       => apply(a, c)(Map(_, b, _))
      case FlatMap(a, b, c)   => apply(a, c)(FlatMap(_, b, _))
      case ConcatMap(a, b, c) => apply(a, c)(ConcatMap(_, b, _))
      case Filter(a, b, c)    => apply(a, c)(Filter(_, b, _))
      case SortBy(a, b, c, d) => apply(a, c)(SortBy(_, b, _, d))
      case GroupBy(a, b, c)   => apply(a, c)(GroupBy(_, b, _))
      case GroupByMap(a, b, c, d, e) =>
        (normalize(a), normalize(c), normalize(e)) match {
          case (`a`, `c`, `e`) => None
          case (a, c, e)       => Some(GroupByMap(a, b, c, d, e))
        }
      case Aggregation(a, b)   => apply(b)(Aggregation(a, _))
      case Take(a, b)          => apply(a, b)(Take.apply)
      case Drop(a, b)          => apply(a, b)(Drop.apply)
      case Union(a, b)         => apply(a, b)(Union.apply)
      case UnionAll(a, b)      => apply(a, b)(UnionAll.apply)
      case Distinct(a)         => apply(a)(Distinct.apply)
      case DistinctOn(a, b, c) => apply(a, c)(DistinctOn(_, b, _))
      case Nested(a)           => apply(a)(Nested.apply)
      case FlatJoin(t, a, iA, on) =>
        (normalize(a), normalize(on)) match {
          case (`a`, `on`) => None
          case (a, on)     => Some(FlatJoin(t, a, iA, on))
        }
      case Join(t, a, b, iA, iB, on) =>
        (normalize(a), normalize(b), normalize(on)) match {
          case (`a`, `b`, `on`) => None
          case (a, b, on)       => Some(Join(t, a, b, iA, iB, on))
        }
    }
  }

  private def apply(a: Ast)(f: Ast => Query) =
    (normalize(a)) match {
      case (`a`) => None
      case (a)   => Some(f(a))
    }

  private def apply(a: Ast, b: Ast)(f: (Ast, Ast) => Query) =
    (normalize(a), normalize(b)) match {
      case (`a`, `b`) => None
      case (a, b)     => Some(f(a, b))
    }

}
