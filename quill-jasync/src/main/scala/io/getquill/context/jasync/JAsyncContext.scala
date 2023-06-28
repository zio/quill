package io.getquill.context.jasync

import com.github.jasync.sql.db.pool.ConnectionPool
import com.github.jasync.sql.db.{ConcreteConnection, Connection, QueryResult, RowData}
import io.getquill.context.sql.SqlContext
import io.getquill.context.sql.idiom.SqlIdiom
import io.getquill.context.{Context, ContextVerbTranslate, ExecutionInfo}
import io.getquill.monad.ScalaFutureIOMonad
import io.getquill.util.ContextLogger
import io.getquill.{NamingStrategy, ReturnAction}
import kotlin.jvm.functions.Function1

import java.time.ZoneId
import java.util.concurrent.CompletableFuture
import scala.compat.java8.FutureConverters
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.jdk.CollectionConverters._
import scala.language.implicitConversions
import scala.util.Try

abstract class JAsyncContext[D <: SqlIdiom, +N <: NamingStrategy, C <: ConcreteConnection](
  val idiom: D,
  val naming: N,
  pool: ConnectionPool[C]
) extends Context[D, N]
    with ContextVerbTranslate
    with SqlContext[D, N]
    with Decoders
    with Encoders
    with ScalaFutureIOMonad {

  private val logger = ContextLogger(classOf[JAsyncContext[_, _, _]])

  override type PrepareRow = Seq[Any]
  override type ResultRow  = RowData
  override type Session    = Unit

  override type Result[T]                        = Future[T]
  override type RunQueryResult[T]                = Seq[T]
  override type RunQuerySingleResult[T]          = T
  override type RunActionResult                  = Long
  override type RunActionReturningResult[T]      = T
  override type RunBatchActionResult             = Seq[Long]
  override type RunBatchActionReturningResult[T] = Seq[T]
  override type NullChecker                      = JasyncNullChecker
  type Runner                                    = Unit

  protected val dateTimeZone = ZoneId.systemDefault()

  class JasyncNullChecker extends BaseNullChecker {
    override def apply(index: Int, row: RowData): Boolean =
      row.get(index) == null
  }
  implicit val nullChecker: NullChecker = new JasyncNullChecker()

  implicit def toFuture[T](cf: CompletableFuture[T]): Future[T] = FutureConverters.toScala(cf)
  implicit def toCompletableFuture[T](f: Future[T]): CompletableFuture[T] =
    FutureConverters.toJava(f).asInstanceOf[CompletableFuture[T]]
  implicit def toKotlinFunction[T, R](f: T => R): Function1[T, R] = new Function1[T, R] {
    override def invoke(t: T): R = f(t)
  }

  override def close = {
    Await.result(pool.disconnect(), Duration.Inf)
    ()
  }

  protected def withConnection[T](f: Connection => Future[T])(implicit ec: ExecutionContext) =
    ec match {
      case TransactionalExecutionContext(ec, conn) => f(conn)
      case other                                   => f(pool)
    }

  protected def extractActionResult[O](returningAction: ReturnAction, extractor: Extractor[O])(
    result: QueryResult
  ): List[O]

  protected def expandAction(sql: String, returningAction: ReturnAction) = sql

  def probe(sql: String) =
    Try {
      Await.result(pool.sendQuery(sql), Duration.Inf)
    }

  def transaction[T](f: TransactionalExecutionContext => Future[T])(implicit ec: ExecutionContext) =
    ec match {
      case tec: TransactionalExecutionContext => toCompletableFuture(f(tec))
      case _ =>
        pool.inTransaction { (c: Connection) =>
          toCompletableFuture(f(TransactionalExecutionContext(ec, c)))
        }
    }

  override def performIO[T](io: IO[T, _], transactional: Boolean = false)(implicit ec: ExecutionContext): Result[T] =
    transactional match {
      case false => super.performIO(io)
      case true  => transaction(super.performIO(io)(_))
    }

  def executeQuery[T](sql: String, prepare: Prepare = identityPrepare, extractor: Extractor[T] = identityExtractor)(
    info: ExecutionInfo,
    dc: Runner
  )(implicit ec: ExecutionContext): Future[List[T]] = {
    val (params, values) = prepare(Nil, ())
    logger.logQuery(sql, params)
    withConnection(_.sendPreparedStatement(sql, values.asJava))
      .map(_.getRows.asScala.iterator.map(row => extractor(row, ())).toList)
  }

  def executeQuerySingle[T](
    sql: String,
    prepare: Prepare = identityPrepare,
    extractor: Extractor[T] = identityExtractor
  )(info: ExecutionInfo, dc: Runner)(implicit ec: ExecutionContext): Future[T] =
    executeQuery(sql, prepare, extractor)(info, dc).map(handleSingleResult(sql, _))

  def executeAction(sql: String, prepare: Prepare = identityPrepare)(info: ExecutionInfo, dc: Runner)(implicit
    ec: ExecutionContext
  ): Future[Long] = {
    val (params, values) = prepare(Nil, ())
    logger.logQuery(sql, params)
    withConnection(_.sendPreparedStatement(sql, values.asJava)).map(_.getRowsAffected)
  }

  def executeActionReturning[T](
    sql: String,
    prepare: Prepare = identityPrepare,
    extractor: Extractor[T],
    returningAction: ReturnAction
  )(info: ExecutionInfo, dc: Runner)(implicit ec: ExecutionContext): Future[T] =
    executeActionReturningMany[T](sql, prepare, extractor, returningAction)(info, dc).map(handleSingleResult(sql, _))

  def executeActionReturningMany[T](
    sql: String,
    prepare: Prepare = identityPrepare,
    extractor: Extractor[T],
    returningAction: ReturnAction
  )(info: ExecutionInfo, dc: Runner)(implicit ec: ExecutionContext): Future[List[T]] = {
    val expanded         = expandAction(sql, returningAction)
    val (params, values) = prepare(Nil, ())
    logger.logQuery(sql, params)
    withConnection(_.sendPreparedStatement(expanded, values.asJava))
      .map(extractActionResult(returningAction, extractor))
  }

  def executeBatchAction(
    groups: List[BatchGroup]
  )(info: ExecutionInfo, dc: Runner)(implicit ec: ExecutionContext): Future[List[Long]] =
    Future.sequence {
      groups.map { case BatchGroup(sql, prepare) =>
        prepare
          .foldLeft(Future.successful(List.newBuilder[Long])) { case (acc, prepare) =>
            acc.flatMap { list =>
              executeAction(sql, prepare)(info, dc).map(list += _)
            }
          }
          .map(_.result())
      }
    }.map(_.flatten.toList)

  def executeBatchActionReturning[T](
    groups: List[BatchGroupReturning],
    extractor: Extractor[T]
  )(info: ExecutionInfo, dc: Runner)(implicit ec: ExecutionContext): Future[List[T]] =
    Future.sequence {
      groups.map { case BatchGroupReturning(sql, column, prepare) =>
        prepare
          .foldLeft(Future.successful(List.newBuilder[T])) { case (acc, prepare) =>
            acc.flatMap { list =>
              executeActionReturning(sql, prepare, extractor, column)(info, dc).map(list += _)
            }
          }
          .map(_.result())
      }
    }.map(_.flatten.toList)

  override private[getquill] def prepareParams(statement: String, prepare: Prepare): Seq[String] =
    prepare(Nil, ())._2.map(prepareParam)

}
