package io.getquill

import com.datastax.driver.core.BoundStatement
import com.datastax.driver.core.Row
import com.typesafe.config.Config
import io.getquill.util.LoadConfig
import io.getquill.context.cassandra.CassandraSessionContext
import scala.collection.JavaConverters._
import com.datastax.driver.core.Cluster
import io.getquill.monad.SyncIOMonad

class CassandraSyncContext[N <: NamingStrategy](
  cluster:                    Cluster,
  keyspace:                   String,
  preparedStatementCacheSize: Long
)
  extends CassandraSessionContext[N](cluster, keyspace, preparedStatementCacheSize)
  with SyncIOMonad {

  def this(config: CassandraContextConfig) = this(config.cluster, config.keyspace, config.preparedStatementCacheSize)
  def this(config: Config) = this(CassandraContextConfig(config))
  def this(configPrefix: String) = this(LoadConfig(configPrefix))

  override type Result[T] = T
  override type RunQueryResult[T] = List[T]
  override type RunQuerySingleResult[T] = T
  override type RunActionResult = Unit
  override type RunBatchActionResult = Unit

  override def unsafePerformIO[T](io: IO[T, _], transactional: Boolean = false): Result[T] = {
    if (transactional) logger.warn("Cassandra doesn't support transactions, ignoring `io.transactional`")
    unsafePerformIO(io)
  }

  def executeQuery[T](cql: String, prepare: BoundStatement => BoundStatement = identity, extractor: Row => T = identity[Row] _): List[T] =
    session.execute(prepare(super.prepare(cql)))
      .all.asScala.toList.map(extractor)

  def executeQuerySingle[T](cql: String, prepare: BoundStatement => BoundStatement = identity, extractor: Row => T = identity[Row] _): T =
    handleSingleResult(executeQuery(cql, prepare, extractor))

  def executeAction[T](cql: String, prepare: BoundStatement => BoundStatement = identity): Unit = {
    session.execute(prepare(super.prepare(cql)))
    ()
  }

  def executeBatchAction(groups: List[BatchGroup]): Unit =
    groups.foreach {
      case BatchGroup(cql, prepare) =>
        prepare.foreach(executeAction(cql, _))
    }
}
