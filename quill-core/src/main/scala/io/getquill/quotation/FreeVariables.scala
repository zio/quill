package io.getquill.quotation

import io.getquill.ast.Aggregation
import io.getquill.ast.Assignment
import io.getquill.ast.Ast
import io.getquill.ast.Distinct
import io.getquill.ast.Drop
import io.getquill.ast.Entity
import io.getquill.ast.Filter
import io.getquill.ast.FlatMap
import io.getquill.ast.Function
import io.getquill.ast.GroupBy
import io.getquill.ast.Ident
import io.getquill.ast.Join
import io.getquill.ast.Map
import io.getquill.ast.OptionOperation
import io.getquill.ast.Query
import io.getquill.ast.SortBy
import io.getquill.ast.StatefulTransformer
import io.getquill.ast.Take
import io.getquill.ast.Union
import io.getquill.ast.UnionAll

case class State(seen: collection.Set[Ident], free: collection.Set[Ident])

case class FreeVariables(state: State)
  extends StatefulTransformer[State] {

  override def apply(ast: Ast): (Ast, StatefulTransformer[State]) =
    ast match {
      case ident: Ident if (!state.seen.contains(ident)) =>
        (ident, FreeVariables(State(state.seen, state.free + ident)))
      case f @ Function(params, body) =>
        val (_, t) = FreeVariables(State(state.seen ++ params, state.free))(body)
        (f, FreeVariables(State(state.seen, state.free ++ t.state.free)))
      case OptionOperation(t, a, b, c) =>
        (ast, free(a, b, c))
      case other =>
        super.apply(other)
    }

  override def apply(query: Query): (Query, StatefulTransformer[State]) =
    query match {
      case q @ Filter(a, b, c)    => (q, free(a, b, c))
      case q @ Map(a, b, c)       => (q, free(a, b, c))
      case q @ FlatMap(a, b, c)   => (q, free(a, b, c))
      case q @ SortBy(a, b, c, d) => (q, free(a, b, c))
      case q @ GroupBy(a, b, c)   => (q, free(a, b, c))
      case q @ Join(t, a, b, iA, iB, on) =>
        val (_, freeA) = apply(a)
        val (_, freeB) = apply(a)
        val (_, freeOn) = FreeVariables(State(state.seen + iA + iB, collection.Set.empty))(on)
        (q, FreeVariables(State(state.seen, state.free ++ freeA.state.free ++ freeB.state.free ++ freeOn.state.free)))
      case _: Entity | _: Take | _: Drop | _: Union | _: UnionAll | _: Aggregation | _: Distinct =>
        super.apply(query)
    }

  override def apply(e: Assignment): (Assignment, StatefulTransformer[State]) =
    e match {
      case Assignment(a, b, c) =>
        val (ct, ctt) = FreeVariables(State(state.seen + a, state.free))(c)
        (Assignment(a, b, ct), ctt)
    }

  private def free(a: Ast, ident: Ident, c: Ast) = {
    val (_, ta) = apply(a)
    val (_, tc) = FreeVariables(State(state.seen + ident, state.free))(c)
    FreeVariables(State(state.seen, state.free ++ ta.state.free ++ tc.state.free))
  }
}

object FreeVariables {
  def apply(ast: Ast): collection.Set[Ident] =
    new FreeVariables(State(collection.Set.empty, collection.Set.empty))(ast) match {
      case (_, transformer) =>
        transformer.state.free
    }
}
