package io.getquill.context.zio

import com.github.jasync.sql.db.{ ConcreteConnection, QueryResult, RowData }
import io.getquill.context.sql.SqlContext
import io.getquill.context.sql.idiom.SqlIdiom
import io.getquill.context.{ Context, ExecutionInfo, TranslateContext }
import io.getquill.util.ContextLogger
import io.getquill.{ NamingStrategy, ReturnAction }
import kotlin.jvm.functions.Function1
import zio.{ Has, RIO, ZIO }

import scala.jdk.CollectionConverters._
import scala.language.implicitConversions
import scala.util.Try

abstract class ZioJAsyncContext[D <: SqlIdiom, N <: NamingStrategy, C <: ConcreteConnection](val idiom: D, val naming: N)
  extends Context[D, N]
  with TranslateContext
  with SqlContext[D, N]
  with Decoders
  with Encoders
  with ZIOMonad {

  private val logger = ContextLogger(classOf[ZioJAsyncContext[_, _, _]])

  override type PrepareRow = Seq[Any]
  override type ResultRow = RowData
  override type Session = Unit

  override type Result[T] = RIO[Has[ZioJAsyncConnection], T]
  override type RunQueryResult[T] = Seq[T]
  override type RunQuerySingleResult[T] = T
  override type RunActionResult = Long
  override type RunActionReturningResult[T] = T
  override type RunBatchActionResult = Seq[Long]
  override type RunBatchActionReturningResult[T] = Seq[T]
  override type DecoderSqlType = SqlTypes.SqlTypes
  type DatasourceContext = Unit

  implicit def toKotlinFunction[T, R](f: T => R): Function1[T, R] = new Function1[T, R] {
    override def invoke(t: T): R = f(t)
  }

  override def close: Unit = {
    //nothing to close since pool is in env
    ()
  }

  protected def extractActionResult[O](returningAction: ReturnAction, extractor: Extractor[O])(result: QueryResult): O

  protected def expandAction(sql: String, returningAction: ReturnAction): String = sql

  def probe(sql: String): Try[_] =
    Try(()) //need to address that

  def transaction[T](action: Result[T]): RIO[Has[ZioJAsyncConnection], T] = {
    ZIO.accessM[Has[ZioJAsyncConnection]](_.get.transaction(action))
  }

  override def performIO[T](io: IO[T, _], transactional: Boolean = false): RIO[Has[ZioJAsyncConnection], T] =
    if (transactional) {
      transaction(super.performIO(io))
    } else {
      super.performIO(io)
    }

  def executeQuery[T](sql: String, prepare: Prepare = identityPrepare, extractor: Extractor[T] = identityExtractor)(info: ExecutionInfo, dc: DatasourceContext): RIO[Has[ZioJAsyncConnection], List[T]] = {
    val (params, values) = prepare(Nil, ())
    logger.logQuery(sql, params)
    ZioJAsyncConnection.sendPreparedStatement(sql, values)
      .map(_.getRows.asScala.iterator.map(row => extractor(row, ())).toList)
  }

  def executeQuerySingle[T](sql: String, prepare: Prepare = identityPrepare, extractor: Extractor[T] = identityExtractor)(info: ExecutionInfo, dc: DatasourceContext): RIO[Has[ZioJAsyncConnection], T] =
    executeQuery(sql, prepare, extractor)(info, dc).map(handleSingleResult)

  def executeAction[T](sql: String, prepare: Prepare = identityPrepare)(info: ExecutionInfo, dc: DatasourceContext): Result[Long] = {
    val (params, values) = prepare(Nil, ())
    logger.logQuery(sql, params)
    ZioJAsyncConnection.sendPreparedStatement(sql, values).map(_.getRowsAffected)
  }

  def executeActionReturning[T](sql: String, prepare: Prepare = identityPrepare, extractor: Extractor[T], returningAction: ReturnAction)(info: ExecutionInfo, dc: DatasourceContext): RIO[Has[ZioJAsyncConnection], T] = {
    val expanded = expandAction(sql, returningAction)
    val (params, values) = prepare(Nil, ())
    logger.logQuery(sql, params)
    ZioJAsyncConnection.sendPreparedStatement(expanded, values)
      .map(extractActionResult(returningAction, extractor))
  }

  def executeBatchAction(groups: List[BatchGroup])(info: ExecutionInfo, dc: DatasourceContext): RIO[Has[ZioJAsyncConnection], List[Long]] =
    ZIO.foreach(groups) { group =>
      ZIO.foldLeft(group.prepare)(List.newBuilder[Long]) {
        case (acc, prepare) =>
          executeAction(group.string, prepare)(info, dc).map(acc += _)
      }.map(_.result())
    }.map(_.flatten.toList)

  def executeBatchActionReturning[T](groups: List[BatchGroupReturning], extractor: Extractor[T])(info: ExecutionInfo, dc: DatasourceContext): RIO[Has[ZioJAsyncConnection], List[T]] =
    ZIO.foreach(groups) {
      case BatchGroupReturning(sql, column, prepare) =>
        ZIO.foldLeft(prepare)(List.newBuilder[T]) {
          case (acc, prepare) =>
            executeActionReturning(sql, prepare, extractor, column)(info, dc).map(acc += _)
        }.map(_.result())
    }.map(_.flatten.toList)

  override private[getquill] def prepareParams(statement: String, prepare: Prepare): Seq[String] =
    prepare(Nil, ())._2.map(prepareParam)

}
