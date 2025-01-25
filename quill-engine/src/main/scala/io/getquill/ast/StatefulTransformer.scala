package io.getquill.ast

import io.getquill.StatefulCache.NoCache
import io.getquill.{HasStatefulCache, StatefulCache}

trait StatefulTransformer[T] extends HasStatefulCache[T] {

  val state: T

  private val defaultCache = NoCache[T]
  // Override this value in implementations in order to use caching
  override def cache: StatefulCache[T] = defaultCache

  def apply(e: Ast): (Ast, StatefulTransformer[T]) = cached(e) {
    e match {
      case e: Query               => apply(e)
      case e: Operation           => apply(e)
      case e: Action              => apply(e)
      case e: Value               => apply(e)
      case e: Assignment          => apply(e)
      case e: AssignmentDual      => apply(e)
      case e: Ident               => (e, this)
      case e: ExternalIdent       => (e, this)
      case e: OptionOperation     => apply(e)
      case e: IterableOperation   => apply(e)
      case e: Property            => apply(e)
      case e: OnConflict.Existing => (e, this)
      case e: OnConflict.Excluded => (e, this)

      case Function(a, b) =>
        val (bt, btt) = apply(b)
        (Function(a, bt), btt)

      case Infix(a, b, pure, transparent, quat) =>
        val (bt, btt) = apply(b)(_.apply)
        (Infix(a, bt, pure, transparent, quat), btt)

      case If(a, b, c) =>
        val (at, att) = apply(a)
        val (bt, btt) = att.apply(b)
        val (ct, ctt) = btt.apply(c)
        (If(at, bt, ct), ctt)

      case l: Dynamic => (l, this)

      case l: External => (l, this)

      case QuotedReference(a, b) =>
        val (bt, btt) = apply(b)
        (QuotedReference(a, bt), btt)

      case Block(a) =>
        val (at, att) = apply(a)(_.apply)
        (Block(at), att)

      case Val(a, b) =>
        val (at, att) = apply(b)
        (Val(a, at), att)

      case o: Ordering => (o, this)
    }
  }

  def apply(o: OptionOperation): (OptionOperation, StatefulTransformer[T]) =
    o match {
      case OptionTableFlatMap(a, b, c) =>
        val (at, att) = apply(a)
        val (ct, ctt) = att.apply(c)
        (OptionTableFlatMap(at, b, ct), ctt)
      case OptionTableMap(a, b, c) =>
        val (at, att) = apply(a)
        val (ct, ctt) = att.apply(c)
        (OptionTableMap(at, b, ct), ctt)
      case OptionTableExists(a, b, c) =>
        val (at, att) = apply(a)
        val (ct, ctt) = att.apply(c)
        (OptionTableExists(at, b, ct), ctt)
      case OptionTableForall(a, b, c) =>
        val (at, att) = apply(a)
        val (ct, ctt) = att.apply(c)
        (OptionTableForall(at, b, ct), ctt)
      case OptionFlatten(a) =>
        val (at, att) = apply(a)
        (OptionFlatten(at), att)
      case OptionGetOrElse(a, c) =>
        val (at, att) = apply(a)
        val (ct, ctt) = att.apply(c)
        (OptionGetOrElse(at, ct), ctt)
      case OptionOrElse(a, c) =>
        val (at, att) = apply(a)
        val (ct, ctt) = att.apply(c)
        (OptionOrElse(at, ct), ctt)
      case OptionFlatMap(a, b, c) =>
        val (at, att) = apply(a)
        val (ct, ctt) = att.apply(c)
        (OptionFlatMap(at, b, ct), ctt)
      case OptionMap(a, b, c) =>
        val (at, att) = apply(a)
        val (ct, ctt) = att.apply(c)
        (OptionMap(at, b, ct), ctt)
      case OptionForall(a, b, c) =>
        val (at, att) = apply(a)
        val (ct, ctt) = att.apply(c)
        (OptionForall(at, b, ct), ctt)
      case OptionExists(a, b, c) =>
        val (at, att) = apply(a)
        val (ct, ctt) = att.apply(c)
        (OptionExists(at, b, ct), ctt)
      case OptionContains(a, c) =>
        val (at, att) = apply(a)
        val (ct, ctt) = att.apply(c)
        (OptionContains(at, ct), ctt)
      case OptionIsEmpty(a) =>
        val (at, att) = apply(a)
        (OptionIsEmpty(at), att)
      case OptionNonEmpty(a) =>
        val (at, att) = apply(a)
        (OptionNonEmpty(at), att)
      case OptionIsDefined(a) =>
        val (at, att) = apply(a)
        (OptionIsDefined(at), att)
      case OptionSome(a) =>
        val (at, att) = apply(a)
        (OptionSome(at), att)
      case OptionApply(a) =>
        val (at, att) = apply(a)
        (OptionApply(at), att)
      case OptionOrNull(a) =>
        val (at, att) = apply(a)
        (OptionOrNull(at), att)
      case OptionGetOrNull(a) =>
        val (at, att) = apply(a)
        (OptionGetOrNull(at), att)
      case FilterIfDefined(a, b, c) =>
        val (at, att) = apply(a)
        val (ct, ctt) = att.apply(c)
        (FilterIfDefined(at, b, ct), ctt)
      case OptionNone(_) => (o, this)
    }

  def apply(e: IterableOperation): (IterableOperation, StatefulTransformer[T]) =
    e match {
      case MapContains(a, c) =>
        val (at, att) = apply(a)
        val (ct, ctt) = att.apply(c)
        (MapContains(at, ct), ctt)
      case SetContains(a, c) =>
        val (at, att) = apply(a)
        val (ct, ctt) = att.apply(c)
        (SetContains(at, ct), ctt)
      case ListContains(a, c) =>
        val (at, att) = apply(a)
        val (ct, ctt) = att.apply(c)
        (ListContains(at, ct), ctt)
    }

  def apply(e: Query): (Query, StatefulTransformer[T]) = cached(e) {
    e match {
      case e: Entity => (e, this)
      case Filter(a, b, c) =>
        val (at, att) = apply(a)
        val (ct, ctt) = att.apply(c)
        (Filter(at, b, ct), ctt)
      case Map(a, b, c) =>
        val (at, att) = apply(a)
        val (ct, ctt) = att.apply(c)
        (Map(at, b, ct), ctt)
      case FlatMap(a, b, c) =>
        val (at, att) = apply(a)
        val (ct, ctt) = att.apply(c)
        (FlatMap(at, b, ct), ctt)
      case ConcatMap(a, b, c) =>
        val (at, att) = apply(a)
        val (ct, ctt) = att.apply(c)
        (ConcatMap(at, b, ct), ctt)
      case SortBy(a, b, c, d) =>
        val (at, att) = apply(a)
        val (ct, ctt) = att.apply(c)
        (SortBy(at, b, ct, d), ctt)
      case GroupBy(a, b, c) =>
        val (at, att) = apply(a)
        val (ct, ctt) = att.apply(c)
        (GroupBy(at, b, ct), ctt)
      case GroupByMap(a, b, c, d, e) =>
        val (at, att) = apply(a)
        val (ct, ctt) = att.apply(c)
        val (et, ett) = ctt.apply(e)
        (GroupByMap(at, b, ct, d, et), ett)
      case Aggregation(o, a) =>
        val (at, att) = apply(a)
        (Aggregation(o, at), att)
      case Take(a, b) =>
        val (at, att) = apply(a)
        val (bt, btt) = att.apply(b)
        (Take(at, bt), btt)
      case Drop(a, b) =>
        val (at, att) = apply(a)
        val (bt, btt) = att.apply(b)
        (Drop(at, bt), btt)
      case Union(a, b) =>
        val (at, att) = apply(a)
        val (bt, btt) = att.apply(b)
        (Union(at, bt), btt)
      case UnionAll(a, b) =>
        val (at, att) = apply(a)
        val (bt, btt) = att.apply(b)
        (UnionAll(at, bt), btt)
      case Join(t, a, b, iA, iB, on) =>
        val (at, att)   = apply(a)
        val (bt, btt)   = att.apply(b)
        val (ont, ontt) = btt.apply(on)
        (Join(t, at, bt, iA, iB, ont), ontt)
      case FlatJoin(t, a, iA, on) =>
        val (at, att)   = apply(a)
        val (ont, ontt) = att.apply(on)
        (FlatJoin(t, at, iA, ont), ontt)
      case Distinct(a) =>
        val (at, att) = apply(a)
        (Distinct(at), att)
      case DistinctOn(a, b, c) =>
        val (at, att) = apply(a)
        val (ct, ctt) = att.apply(c)
        (DistinctOn(at, b, ct), ctt)
      case Nested(a) =>
        val (at, att) = apply(a)
        (Nested(at), att)
    }
  }

  def apply(e: Assignment): (Assignment, StatefulTransformer[T]) =
    e match {
      case Assignment(a, b, c) =>
        val (bt, btt) = apply(b)
        val (ct, ctt) = btt.apply(c)
        (Assignment(a, bt, ct), ctt)
    }

  def apply(e: AssignmentDual): (AssignmentDual, StatefulTransformer[T]) =
    e match {
      case AssignmentDual(a1, a2, b, c) =>
        val (bt, btt) = apply(b)
        val (ct, ctt) = btt.apply(c)
        (AssignmentDual(a1, a2, bt, ct), ctt)
    }

  def apply(e: Property): (Property, StatefulTransformer[T]) =
    e match {
      case Property.Opinionated(a, b, renameable, visibility) =>
        val (at, att) = apply(a)
        (Property.Opinionated(at, b, renameable, visibility), att)
    }

  def apply(e: Operation): (Operation, StatefulTransformer[T]) =
    e match {
      case UnaryOperation(o, a) =>
        val (at, att) = apply(a)
        (UnaryOperation(o, at), att)
      case BinaryOperation(a, b, c) =>
        val (at, att) = apply(a)
        val (ct, ctt) = att.apply(c)
        (BinaryOperation(at, b, ct), ctt)
      case FunctionApply(a, b) =>
        val (at, att) = apply(a)
        val (bt, btt) = att.apply(b)(_.apply)
        (FunctionApply(at, bt), btt)
    }

  def apply(e: Value): (Value, StatefulTransformer[T]) =
    e match {
      case e: Constant => (e, this)
      case NullValue   => (e, this)
      case Tuple(a) =>
        val (at, att) = apply(a)(_.apply)
        (Tuple(at), att)
      case CaseClass(n, a) =>
        val (keys, values) = a.unzip
        val (at, att)      = apply(values)(_.apply)
        (CaseClass(n, keys.zip(at)), att)
    }

  def apply(e: Action): (Action, StatefulTransformer[T]) =
    e match {
      case Insert(a, b) =>
        val (at, att) = apply(a)
        val (bt, btt) = att.apply(b)(_.apply)
        (Insert(at, bt), btt)
      case Update(a, b) =>
        val (at, att) = apply(a)
        val (bt, btt) = att.apply(b)(_.apply)
        (Update(at, bt), btt)
      case Delete(a) =>
        val (at, att) = apply(a)
        (Delete(at), att)
      case Returning(a, b, c) =>
        val (at, att) = apply(a)
        val (ct, ctt) = att.apply(c)
        (Returning(at, b, ct), ctt)
      case ReturningGenerated(a, b, c) =>
        val (at, att) = apply(a)
        val (ct, ctt) = att.apply(c)
        (ReturningGenerated(at, b, ct), ctt)
      case Foreach(a, b, c) =>
        val (at, att) = apply(a)
        val (ct, ctt) = att.apply(c)
        (Foreach(at, b, ct), ctt)
      case OnConflict(a, b, c) =>
        val (at, att) = apply(a)
        val (bt, btt) = att.apply(b)
        val (ct, ctt) = btt.apply(c)
        (OnConflict(at, bt, ct), ctt)
    }

  def apply(e: OnConflict.Target): (OnConflict.Target, StatefulTransformer[T]) =
    e match {
      case OnConflict.NoTarget => (e, this)
      case OnConflict.Properties(a) =>
        val (at, att) = apply(a)(_.apply)
        (OnConflict.Properties(at), att)
    }

  def apply(e: OnConflict.Action): (OnConflict.Action, StatefulTransformer[T]) =
    e match {
      case OnConflict.Ignore => (e, this)
      case OnConflict.Update(a) =>
        val (at, att) = apply(a)(_.apply)
        (OnConflict.Update(at), att)
    }

  def apply[U, R](
    list: List[U]
  )(f: StatefulTransformer[T] => U => (R, StatefulTransformer[T])): (List[R], StatefulTransformer[T]) = {
    val (builder, transformer) =
      list.foldLeft((List.newBuilder[R], this)) { case ((values, t), v) =>
        val (vt, vtt) = f(t)(v)
        (values += vt, vtt)
      }

    (builder.result(), transformer)
  }
}
