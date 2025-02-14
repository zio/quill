package io.getquill.context.sql.norm

import io.getquill.{HasStatelessCache, StatelessCache}
import io.getquill.ast._
import io.getquill.norm.BetaReduction
import io.getquill.norm.Normalize

/**
 * This phase expands inner joins adding the correct aliases so they will
 * function. Unfortunately, since it introduces aliases into the clauses that
 * don't actually exist in the inner expressions, it is not technically
 * type-safe but will not result in a Quat error since Quats cannot check for
 * Ident scoping. For a better implementation, that uses a well-typed
 * FlatMap/FlatJoin cascade, have a look here:
 * [[https://gist.github.com/deusaquilus/dfb42880656df12779a0afd4f20ef1bb Better Typed ExpandJoin which uses FlatMap/FlatJoin]]
 *
 * The reason the above implementation is not currently used is because
 * `ExpandNestedQueries` does not yet use Quat fields for expansion. Once this
 * is changed, using that implementation here should be reconsidered.
 */
class ExpandJoin(val cache: StatelessCache, normalize: Normalize) extends HasStatelessCache {

  def apply(q: Ast) = cached(q) {
    expand(q, None)
  }

  def expand(q: Ast, id: Option[Ident]) =
    Transform(q) {
      case q @ Join(
            _,
            _,
            _,
            Ident(a, _),
            Ident(b, _),
            _
          ) => // Ident a and Ident b should have the same Quat, could add an assertion for that
        val (qr, tuple) = expandedTuple(q)
        Map(qr, id.getOrElse(Ident(s"$a$b", q.quat)), tuple)
    }

  private def expandedTuple(q: Join): (Join, Tuple) =
    q match {

      case Join(t, a: Join, b: Join, tA, tB, o) =>
        val (ar, at) = expandedTuple(a)
        val (br, bt) = expandedTuple(b)
        val or       = BetaReduction(o, tA -> at, tB -> bt)
        (Join(t, ar, br, tA, tB, or), Tuple(List(at, bt)))

      case Join(t, a: Join, b, tA, tB, o) =>
        val (ar, at) = expandedTuple(a)
        val or       = BetaReduction(o, tA -> at)
        (Join(t, ar, b, tA, tB, or), Tuple(List(at, tB)))

      case Join(t, a, b: Join, tA, tB, o) =>
        val (br, bt) = expandedTuple(b)
        val or       = BetaReduction(o, tB -> bt)
        (Join(t, a, br, tA, tB, or), Tuple(List(tA, bt)))

      case q @ Join(t, a, b, tA, tB, on) =>
        (Join(t, nestedExpand(a, tA), nestedExpand(b, tB), tA, tB, on), Tuple(List(tA, tB)))
    }

  private def nestedExpand(q: Ast, id: Ident) =
    normalize(expand(q, Some(id))) match {
      case Map(q, _, _) => q
      case q            => q
    }
}
