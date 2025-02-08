package io.getquill.norm

import io.getquill.{HasStatelessCacheOpt, StatelessCacheOpt}
import io.getquill.ast.{Filter, FlatMap, Query, Union, UnionAll}
import io.getquill.util.TraceConfig

/**
 * This stage represents Normalization Stage1: Symbolic Reduction in Philip
 * Wadler's Paper "A Practical Theory of Language Integrated Query", given in
 * Figure 11.
 * https://homepages.inf.ed.ac.uk/slindley/papers/practical-theory-of-linq.pdf
 *
 * It represents foundational normalizations done to sequences that represents
 * queries. In Wadler's paper, he characterizes them as `for x in P ...``
 * whereas in Quill they are characterized as list comprehensions i.e.
 * `P.flatMap(x => ...)`.
 */
class SymbolicReduction(val cache: StatelessCacheOpt, traceConfig: TraceConfig) extends HasStatelessCacheOpt {

  def unapply(q: Query) = cached(q) {
    q match {

      /*
       * Represents if-for in Figure 11.
       * This transformation is particularity difficult to understand so I have added examples with several layers of granularity.
       * It's basic form goes like this:
       *
       * bottles.filter(bottle => isMerlot).flatMap(merlotBottle => {withCheese})
       *     bottles.flatMap(merlotBottle => {withCheese}.filter(_ => isMerlot[bottle := merlotBottle]))
       *
       * For example:
       *   The AST block withCheese is cheeses.filter(cheese => cheese.pairsWith == merlotBottle)
       *   The AST block isMerlot is bottle.isMerlot
       *
       * bottles.filter(bottle => bottle.isMerlot).flatMap(merlotBottle => {cheeses.filter(cheese => cheese.pairsWith == merlotBottle)} )
       *     bottles.flatMap(merlotBottle => cheeses.filter({bottle.isMerlot}[bottle := merlotBottle]).filter(cheese => cheese.pairsWith == merlotBottle)}
       * which is:
       *     bottles.flatMap(merlotBottle => cheeses.filter({doesnt-matter} => merlotBottle.isMerlot).filter(cheese => cheese.pairsWith == merlotBottle)
       *
       * a.filter(b => c).flatMap(d => e.$) =>
       *     a.flatMap(d => e.filter(_ => c[b := d]).$)
       */
      case FlatMap(Filter(a, b, c), d, e: Query) =>
        val cr = BetaReduction(c, b -> d)
        val er = AttachToEntity(Filter(_, _, cr))(e)
        Some(FlatMap(a, d, er))

      // This transformation does not have an analogue in Wadler's paper, it represents the fundamental nature of the Monadic 'bind' function
      // that A.flatMap(a => B).flatMap(b => C) is isomorphic to A.flatMap(a => B.flatMap(b => C)).
      //
      // a.flatMap(b => c).flatMap(d => e) =>
      //     a.flatMap(b => c.flatMap(d => e))
      case FlatMap(FlatMap(a, b, c), d, e) =>
        Some(FlatMap(a, b, FlatMap(c, d, e)))

      // Represents for@ in Figure 11
      //
      // a.union(b).flatMap(c => d)
      //      a.flatMap(c => d).union(b.flatMap(c => d))
      case FlatMap(Union(a, b), c, d) =>
        Some(Union(FlatMap(a, c, d), FlatMap(b, c, d)))

      // Represents for@ in Figure 11 (Wadler does not distinguish between Union and UnionAll)
      //
      // a.unionAll(b).flatMap(c => d)
      //      a.flatMap(c => d).unionAll(b.flatMap(c => d))
      case FlatMap(UnionAll(a, b), c, d) =>
        Some(UnionAll(FlatMap(a, c, d), FlatMap(b, c, d)))

      case other => None
    }
  }
}
