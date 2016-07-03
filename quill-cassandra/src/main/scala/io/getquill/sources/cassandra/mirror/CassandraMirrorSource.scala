package io.getquill.sources.cassandra.mirror

import io.getquill.naming.Literal
import io.getquill.sources.cassandra.CassandraSource
import io.getquill.sources.mirror.Row
import io.getquill.sources.mirror.MirrorEncoders
import io.getquill.sources.mirror.MirrorDecoders
import scala.util.Failure
import scala.util.Success
import io.getquill.QueryProbing

object cassandraMirrorSource extends CassandraMirrorSourceTemplate with QueryProbing

class CassandraMirrorSourceTemplate
  extends CassandraSource[Literal, Row, Row]
  with MirrorEncoders
  with MirrorDecoders {

  override type QueryResult[T] = QueryMirror[T]
  override type SingleQueryResult[T] = QueryMirror[T]
  override type ActionResult[T] = ActionMirror
  override type BatchedActionResult[T] = BatchActionMirror
  override type Params[T] = List[T]

  class ActionApply[T](f: Params[T] => BatchActionMirror) extends (Params[T] => BatchActionMirror) {
    def apply(params: Params[T]) = f(params)
    def apply(param: T) = ActionMirror(f(List(param)).cql, Row(param))
  }

  override def close = ()

  override def probe(cql: String) =
    if (cql.contains("fail"))
      Failure(new IllegalStateException)
    else
      Success(())

  case class ActionMirror(cql: String, bind: Row)

  def executeAction(cql: String, bind: Row => Row = identity, generated: Option[String] = None) =
    ActionMirror(cql, bind(Row()))

  case class BatchActionMirror(cql: String, bindList: List[Row])

  def executeActionBatch[T](cql: String, bindParams: T => Row => Row = (_: T) => identity[Row] _, generated: Option[String] = None) = {
    val f = (values: List[T]) =>
      BatchActionMirror(cql, values.map(bindParams).map(_(Row())))
    new ActionApply[T](f)
  }

  case class QueryMirror[T](cql: String, binds: Row, extractor: Row => T)

  def executeQuery[T](cql: String, extractor: Row => T = identity[Row] _, bind: Row => Row = identity) =
    QueryMirror(cql, bind(Row()), extractor)

  def executeQuerySingle[T](cql: String, extractor: Row => T = identity[Row] _, bind: Row => Row = identity) =
    executeQuery(cql, extractor, bind)
}
