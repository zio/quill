package io.getquill.quotation

import io.getquill.ast._

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
      case q @ Foreach(a, b, c) =>
        (q, free(a, b, c))
      case other =>
        super.apply(other)
    }

  override def apply(e: Assignment): (Assignment, StatefulTransformer[State]) =
    e match {
      case Assignment(a, b, c) =>
        val t = FreeVariables(State(state.seen + a, state.free))
        val (bt, btt) = t(b)
        val (ct, ctt) = t(c)
        (Assignment(a, bt, ct), FreeVariables(State(state.seen, state.free ++ btt.state.free ++ ctt.state.free)))
    }

  override def apply(action: Action): (Action, StatefulTransformer[State]) =
    action match {
      case q @ Returning(a, b, c) =>
        (q, free(a, b, c))
      case other =>
        super.apply(other)
    }

  override def apply(query: Query): (Query, StatefulTransformer[State]) =
    query match {
      case q @ Filter(a, b, c)      => (q, free(a, b, c))
      case q @ Map(a, b, c)         => (q, free(a, b, c))
      case q @ FlatMap(a, b, c)     => (q, free(a, b, c))
      case q @ SortBy(a, b, c, d)   => (q, free(a, b, c))
      case q @ GroupBy(a, b, c)     => (q, free(a, b, c))
      case q @ FlatJoin(t, a, b, c) => (q, free(a, b, c))
      case q @ Join(t, a, b, iA, iB, on) =>
        val (_, freeA) = apply(a)
        val (_, freeB) = apply(b)
        val (_, freeOn) = FreeVariables(State(state.seen + iA + iB, collection.Set.empty))(on)
        (q, FreeVariables(State(state.seen, state.free ++ freeA.state.free ++ freeB.state.free ++ freeOn.state.free)))
      case _: Entity | _: Take | _: Drop | _: Union | _: UnionAll | _: Aggregation | _: Distinct | _: Nested =>
        super.apply(query)
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
