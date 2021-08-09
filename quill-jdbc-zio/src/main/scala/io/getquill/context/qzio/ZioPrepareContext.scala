package io.getquill.context.qzio

import io.getquill.NamingStrategy
import io.getquill.context.PrepareContext
import io.getquill.context.ZioJdbc._
import io.getquill.context.sql.idiom.SqlIdiom
import io.getquill.util.ContextLogger
import zio.{ Has, Task, ZIO }

import java.sql.{ Connection, PreparedStatement, ResultSet, SQLException }

trait ZioPrepareContext[Dialect <: SqlIdiom, Naming <: NamingStrategy] extends ZioContext[Dialect, Naming]
  with PrepareContext {

  private[getquill] val logger = ContextLogger(classOf[ZioPrepareContext[_, _]])

  override type PrepareRow = PreparedStatement
  override type ResultRow = ResultSet
  override type PrepareQueryResult = QIO[PrepareRow]
  override type PrepareActionResult = QIO[PrepareRow]
  override type PrepareBatchActionResult = QIO[List[PrepareRow]]
  override type Session = Connection

  def prepareQuery[T](sql: String, prepare: Prepare = identityPrepare, extractor: Extractor[T] = identityExtractor): PrepareQueryResult =
    prepareSingle(sql, prepare)

  def prepareAction(sql: String, prepare: Prepare = identityPrepare): PrepareActionResult =
    prepareSingle(sql, prepare)

  /** Execute SQL on connection and return prepared statement. Closes the statement in a bracket. */
  def prepareSingle(sql: String, prepare: Prepare = identityPrepare): QIO[PreparedStatement] = {
    (for {
      bconn <- ZIO.environment[Has[Connection]]
      conn = bconn.get[Connection]
      stmt <- Task(conn.prepareStatement(sql))
      ps <- Task {
        val (params, ps) = prepare(stmt, conn)
        logger.logQuery(sql, params)
        ps
      }
    } yield ps).refineToOrDie[SQLException]
  }

  def prepareBatchAction(groups: List[BatchGroup]): PrepareBatchActionResult =
    ZIO.collectAll[Has[Connection], Throwable, PrepareRow, List] {
      val batches = groups.flatMap {
        case BatchGroup(sql, prepares) =>
          prepares.map(sql -> _)
      }
      batches.map {
        case (sql, prepare) =>
          prepareSingle(sql, prepare)
      }
    }.refineToOrDie[SQLException]
}
