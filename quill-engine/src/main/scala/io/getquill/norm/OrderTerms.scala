package io.getquill.norm

import io.getquill.{HasStatelessCacheOpt, StatelessCacheOpt}
import io.getquill.ast._
import io.getquill.util.TraceConfig

class OrderTerms(val cache: StatelessCacheOpt, traceConfig: TraceConfig) extends HasStatelessCacheOpt {

  def unapply(q: Query) = cached(q) {
    q match {

      case Take(Map(a: GroupBy, b, c), d) => None

      // a.sortBy(b => c).filter(d => e) =>
      //     a.filter(d => e).sortBy(b => c)
      case Filter(SortBy(a, b, c, d), e, f) =>
        Some(SortBy(Filter(a, e, f), b, c, d))

      // a.flatMap(b => c).take(n).map(d => e) =>
      //   a.flatMap(b => c).map(d => e).take(n)
      case Map(Take(fm: FlatMap, n), ma, mb) =>
        Some(Take(Map(fm, ma, mb), n))

      // a.flatMap(b => c).drop(n).map(d => e) =>
      //   a.flatMap(b => c).map(d => e).drop(n)
      case Map(Drop(fm: FlatMap, n), ma, mb) =>
        Some(Drop(Map(fm, ma, mb), n))

      case other => None
    }
  }
}
