package io.getquill

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import com.datastax.driver.core.BoundStatement
import com.datastax.driver.core.Row
import io.getquill.context.cassandra.util.FutureConversions.toScalaFuture
import io.getquill.util.LoadConfig
import com.typesafe.config.Config
import scala.collection.JavaConverters._
import io.getquill.context.cassandra.CassandraSessionContext
import com.datastax.driver.core.Cluster
import io.getquill.monad.ScalaFutureIOMonad

class CassandraAsyncContext[N <: NamingStrategy](
  cluster:                    Cluster,
  keyspace:                   String,
  preparedStatementCacheSize: Long
)
  extends CassandraSessionContext[N](cluster, keyspace, preparedStatementCacheSize)
  with ScalaFutureIOMonad {

  def this(config: CassandraContextConfig) = this(config.cluster, config.keyspace, config.preparedStatementCacheSize)
  def this(config: Config) = this(CassandraContextConfig(config))
  def this(configPrefix: String) = this(LoadConfig(configPrefix))

  override type Result[T] = Future[T]
  override type RunQueryResult[T] = List[T]
  override type RunQuerySingleResult[T] = T
  override type RunActionResult = Unit
  override type RunBatchActionResult = Unit

  override def unsafePerformIO[T](io: IO[T, _], transactional: Boolean = false)(implicit ec: ExecutionContext): Result[T] = {
    if (transactional) logger.warn("Cassandra doesn't support transactions, ignoring `io.transactional`")
    unsafePerformIO(io)
  }

  def executeQuery[T](cql: String, prepare: BoundStatement => BoundStatement = identity, extractor: Row => T = identity[Row] _)(implicit ec: ExecutionContext): Future[List[T]] =
    session.executeAsync(prepare(super.prepare(cql)))
      .map(_.all.asScala.toList.map(extractor))

  def executeQuerySingle[T](cql: String, prepare: BoundStatement => BoundStatement = identity, extractor: Row => T = identity[Row] _)(implicit ec: ExecutionContext): Future[T] =
    executeQuery(cql, prepare, extractor).map(handleSingleResult)

  def executeAction[T](cql: String, prepare: BoundStatement => BoundStatement = identity)(implicit ec: ExecutionContext): Future[Unit] =
    session.executeAsync(prepare(super.prepare(cql))).map(_ => ())

  def executeBatchAction(groups: List[BatchGroup])(implicit ec: ExecutionContext): Future[Unit] =
    Future.sequence {
      groups.map {
        case BatchGroup(cql, prepare) =>
          prepare.map(executeAction(cql, _))
      }.flatten
    }.map(_ => ())
}
