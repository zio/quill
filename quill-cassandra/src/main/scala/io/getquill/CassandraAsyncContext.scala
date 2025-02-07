package io.getquill

import com.datastax.oss.driver.api.core.CqlSession
import com.datastax.oss.driver.api.core.cql.AsyncResultSet
import com.typesafe.config.Config
import io.getquill.context.ExecutionInfo
import io.getquill.monad.ScalaFutureIOMonad
import io.getquill.util.{ContextLogger, LoadConfig}

import scala.compat.java8.FutureConverters._
import scala.concurrent.{ExecutionContext, Future}
import scala.jdk.CollectionConverters._

class CassandraAsyncContext[+N <: NamingStrategy](
  naming: N,
  session: CqlSession,
  preparedStatementCacheSize: Long
) extends CassandraCqlSessionContext[N](naming, session, preparedStatementCacheSize)
    with ScalaFutureIOMonad {

  def this(naming: N, config: CassandraContextConfig) = this(naming, config.session, config.preparedStatementCacheSize)

  def this(naming: N, config: Config) = this(naming, CassandraContextConfig(config))

  def this(naming: N, configPrefix: String) = this(naming, LoadConfig(configPrefix))

  private val logger = ContextLogger(classOf[CassandraAsyncContext[_]])

  override type Result[T]               = Future[T]
  override type RunQueryResult[T]       = List[T]
  override type RunQuerySingleResult[T] = T
  override type RunActionResult         = AsyncResultSet
  override type RunBatchActionResult    = Unit
  override type Runner                  = Unit

  override def performIO[T](io: IO[T, _], transactional: Boolean = false)(implicit ec: ExecutionContext): Result[T] = {
    if (transactional) logger.underlying.warn("Cassandra doesn't support transactions, ignoring `io.transactional`")
    super.performIO(io)
  }

  def executeQuery[T](cql: String, prepare: Prepare = identityPrepare, extractor: Extractor[T] = identityExtractor)(
    info: ExecutionInfo,
    dc: Runner
  )(implicit executionContext: ExecutionContext): Result[RunQueryResult[T]] = {
    val statement = prepareAsyncAndGetStatement(cql, prepare, this, logger)
    statement.map(st => session.execute(st).asScala.toList.map(row => extractor(row, this)))
  }

  def executeQuerySingle[T](
    cql: String,
    prepare: Prepare = identityPrepare,
    extractor: Extractor[T] = identityExtractor
  )(info: ExecutionInfo, dc: Runner)(implicit executionContext: ExecutionContext): Result[RunQuerySingleResult[T]] =
    executeQuery(cql, prepare, extractor)(info, dc).map(handleSingleResult(cql, _))

  def executeAction(cql: String, prepare: Prepare = identityPrepare)(info: ExecutionInfo, dc: Runner)(implicit
    executionContext: ExecutionContext
  ): Result[RunActionResult] = {
    val statement = prepareAsyncAndGetStatement(cql, prepare, this, logger)
    statement.flatMap(st => session.executeAsync(st).toCompletableFuture.toScala)
  }

  def executeBatchAction(
    groups: List[BatchGroup]
  )(info: ExecutionInfo, dc: Runner)(implicit executionContext: ExecutionContext): Result[RunBatchActionResult] =
    Future.sequence {
      groups.flatMap { case BatchGroup(cql, prepare, _) =>
        prepare.map(executeAction(cql, _)(info, dc))
      }
    }.map(_ => ())

}
