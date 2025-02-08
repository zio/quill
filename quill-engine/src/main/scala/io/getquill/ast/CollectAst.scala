package io.getquill.ast

import scala.collection.immutable.Queue
import scala.reflect.ClassTag

case class VisitAst(p: PartialFunction[Ast, Unit]) extends StatelessTransformer {
  override def apply(a: Ast) = {
    p.lift(a).getOrElse(())
    super.apply(a)
  }
}

case class QueryStats(
  hasOptionOps: Boolean,
  hasFlatMaps: Boolean,
  hasDistincts: Boolean,
  hasRenames: Boolean,
  hasJoins: Boolean,
  hasNullValues: Boolean,
  hasInfixes: Boolean,
  hasReturning: Boolean
)

class CollectStats {
  var hasOptionOps: Boolean  = false
  var hasFlatMaps: Boolean   = false
  var hasDistincts: Boolean  = false
  var hasRenames: Boolean    = false
  var hasJoins: Boolean      = false
  var hasNullValues: Boolean = false
  var hasInfixes: Boolean    = false
  var hasReturning: Boolean  = false

  def apply(ast: Ast): QueryStats = {
    VisitAst {
      case _: OptionOperation    => hasOptionOps = true
      case _: FlatMap            => hasFlatMaps = true
      case _: Distinct           => hasDistincts = true
      case _: Join               => hasJoins = true
      case _: NullValue.type     => hasNullValues = true
      case _: Infix              => hasInfixes = true
      case _: Returning          => hasReturning = true
      case _: ReturningGenerated => hasReturning = true
      case e: Entity if (e.properties.nonEmpty) =>
        hasRenames = true
    }.apply(ast)

    QueryStats(hasOptionOps, hasFlatMaps, hasDistincts, hasRenames, hasJoins, hasNullValues, hasInfixes, hasReturning)
  }
}
object CollectStats {
  def apply(ast: Ast): QueryStats = new CollectStats().apply(ast)
}

/**
 * The collection is treated as immutable internally but an ArrayBuffer is more
 * efficient then Collection.list at appending which is mostly what the
 * collection does
 */
class CollectAst[T](p: PartialFunction[Ast, T], val state: Queue[T]) extends StatefulTransformer[Queue[T]] {

  override def apply(a: Ast): (Ast, StatefulTransformer[Queue[T]]) =
    a match {
      case d if (p.isDefinedAt(d)) => (d, new CollectAst(p, state :+ p(d)))
      case other                   => super.apply(other)
    }
}

object CollectAst {

  def byType[T: ClassTag](a: Ast): Queue[T] =
    apply[T](a) { case t: T =>
      t
    }

  def apply[T](a: Ast)(p: PartialFunction[Ast, T]): Queue[T] =
    new CollectAst(p, Queue.empty[T]).apply(a) match {
      case (_, transformer) =>
        transformer.state
    }
}
