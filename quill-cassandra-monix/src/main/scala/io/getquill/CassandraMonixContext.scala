package io.getquill

import com.datastax.oss.driver.api.core.CqlSession
import com.datastax.oss.driver.api.core.cql._
import com.restore.cassandra.baselineMigration.migration.Logging
import com.typesafe.config.Config
import io.getquill.context.ExecutionInfo
import io.getquill.context.cassandra.CqlIdiom
import io.getquill.context.monix.MonixContext
import io.getquill.util.{ ContextLogger, LoadConfig }
import io.getquill.{ CassandraContextConfig, CassandraCqlSessionContext, NamingStrategy }
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable

import scala.compat.java8.FutureConverters._
import scala.jdk.CollectionConverters._
import scala.util.{ Failure, Success }

class CassandraMonixContext[+N <: NamingStrategy](
   naming: N,
   session: CqlSession,
   preparedStatementCacheSize: Long
) extends CassandraCqlSessionContext[N](
     naming,
     session,
     preparedStatementCacheSize
   )
   with MonixContext[CqlIdiom, N]
   with Logging {

  def this(naming: N, config: CassandraContextConfig) =
    this(naming, config.session, config.preparedStatementCacheSize)
  def this(naming: N, config: Config) =
    this(naming, CassandraContextConfig(config))
  def this(naming: N, configPrefix: String) =
    this(naming, LoadConfig(configPrefix))

  private val logger = ContextLogger(classOf[CassandraMonixContext[_]])

  override type StreamResult[T] = Observable[T]
  override type RunActionResult = Unit
  override type Result[T] = Task[T]

  override type RunQueryResult[T] = List[T]
  override type RunQuerySingleResult[T] = T
  override type RunBatchActionResult = Unit

  protected def page(rs: AsyncResultSet): Task[Iterable[Row]] =
    Task.defer {
      val page = rs.currentPage().asScala
      if (rs.hasMorePages)
        Task
          .from(rs.fetchNextPage().toCompletableFuture.toScala)
          .map(next => page)
      else {
        Task.now(page)
      }
    }

  def streamQuery[T](
     fetchSize: Option[Int],
     cql: String,
     prepare: Prepare = identityPrepare,
     extractor: Extractor[T] = identityExtractor
  )(
     info: ExecutionInfo,
     dc: Runner
  ): Observable[T] = {
    Observable
      .fromTask(prepareRowAndLog(cql, prepare, fetchSize))
      .mapEvalF(p => session.executeAsync(p).toScala)
      .flatMap(x =>
        Observable.fromAsyncStateAction { (rs: Option[AsyncResultSet]) =>
          rs match {
            case None => Task.now(Iterable.empty -> None)
            case Some(rs) if rs.hasMorePages => {
              val page = rs.currentPage()
              Task
                .fromFuture(rs.fetchNextPage().toScala)
                .map(rs => page.asScala -> Some(rs))
            }
            case Some(rs) =>
              val page = rs.currentPage()
              Task.now(page.asScala -> None)
          }
        }(Some(x))
      )
      .takeWhile(_.nonEmpty)
      .flatMap(Observable.fromIterable)
      .map(row => extractor(row, this))
  }

  def executeQuery[T](
     cql: String,
     prepare: Prepare = identityPrepare,
     extractor: Extractor[T] = identityExtractor
  )(
     info: ExecutionInfo,
     dc: Runner
  ): Task[List[T]] = {
    streamQuery[T](None, cql, prepare, extractor)(info, dc)
      .foldLeftL(List[T]()) { case (l, r) => r +: l }
      .map(_.reverse)
  }

  def executeQuerySingle[T](
     cql: String,
     prepare: Prepare = identityPrepare,
     extractor: Extractor[T] = identityExtractor
  )(
     info: ExecutionInfo,
     dc: Runner
  ): Task[T] =
    executeQuery(cql, prepare, extractor)(info, dc).map(
      handleSingleResult(cql, _)
    )

  def executeAction(
     cql: String,
     prepare: Prepare = identityPrepare
  )(
     info: ExecutionInfo,
     dc: Runner
  ): Task[Unit] = {
    prepareRowAndLog(cql, prepare)
      .flatMap(r => Task.fromFuture(session.executeAsync(r).toScala))
      .map(_ => ())
  }

  def executeBatchAction(
     groups: List[BatchGroup]
  )(
     info: ExecutionInfo,
     dc: Runner
  ): Task[Unit] =
    Observable
      .fromIterable(groups)
      .flatMap { case BatchGroup(cql, prepare) =>
        Observable
          .fromTask {
            Task
              .async0[PrepareRow] { (scheduler, callback) =>
                super
                  .prepareAsync(cql)(scheduler)
                  .onComplete { case Success(statement) =>
                    callback.onSuccess(statement)

                  }(scheduler)
              }
              .flatMap { statements =>
                val preparedStatement =
                  prepare.map(_.apply(statements, this)._2)

                Task.fromFuture {
                  val batch = BatchStatement
                    .builder(DefaultBatchType.UNLOGGED)
                    .addStatements(
                      preparedStatement.asJava
                        .asInstanceOf[java.lang.Iterable[BatchableStatement[_]]]
                    )
                    .build()
                  session.executeAsync(batch).toScala
                }
              }
          }
      }
      .map(_ => ())
      .completedL

  private def prepareRowAndLog(
     cql: String,
     prepare: Prepare = identityPrepare,
     fetchSize: Option[Int] = None
  ): Task[PrepareRow] = {
    Task.async0[PrepareRow] { (scheduler, callback) =>
      implicit val executor: Scheduler = scheduler

      super
        .prepareAsync(cql)
        .map { row =>
          val rowWithPageSize = fetchSize match {
            case Some(size) =>
              row.setPageSize(size)
            case None => row
          }
          prepare(rowWithPageSize, this)
        }
        .onComplete {
          case Success((params, bs)) =>
            logger.logQuery(cql, params)
            callback.onSuccess(bs)
          case Failure(ex) =>
            callback.onError(ex)
        }
    }
  }
}
