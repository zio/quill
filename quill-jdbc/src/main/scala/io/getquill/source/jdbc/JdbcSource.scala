package io.getquill.source.jdbc

import java.sql.Connection
import java.sql.PreparedStatement
import java.sql.ResultSet
import scala.util.DynamicVariable
import com.typesafe.scalalogging.StrictLogging
import io.getquill.naming.NamingStrategy
import io.getquill.source.sql.SqlSource
import scala.util.Try
import io.getquill.source.sql.idiom.SqlIdiom
import scala.util.control.NonFatal
import scala.annotation.tailrec

class JdbcSource[D <: SqlIdiom, N <: NamingStrategy]
    extends SqlSource[D, N, ResultSet, PreparedStatement]
    with JdbcEncoders
    with JdbcDecoders
    with StrictLogging {

  type QueryResult[T] = List[T]
  type ActionResult[T] = Int
  type BatchedActionResult[T] = List[Int]

  protected def createDataSource = DataSource(config)

  protected val dataSource = createDataSource

  override def close = dataSource.close

  private val currentConnection = new DynamicVariable[Option[Connection]](None)

  protected def withConnection[T](f: Connection => T) =
    currentConnection.value.map(f).getOrElse {
      val conn = dataSource.getConnection
      try f(conn)
      finally conn.close
    }

  def probe(sql: String) =
    withConnection { conn =>
      Try {
        conn.createStatement.execute(sql)
      }
    }

  def transaction[T](f: => T) =
    withConnection { conn =>
      currentConnection.withValue(Some(conn)) {
        conn.setAutoCommit(false)
        try {
          val res = f
          conn.commit
          res
        } catch {
          case NonFatal(e) =>
            conn.rollback
            throw e
        } finally {
          conn.setAutoCommit(true)
        }
      }
    }

  def execute(sql: String): Int = {
    logger.info(sql)
    withConnection {
      _.prepareStatement(sql).executeUpdate
    }
  }

  def execute(sql: String, bindList: List[PreparedStatement => PreparedStatement]): List[Int] = {
    logger.info(sql)
    withConnection { conn =>
      val ps = conn.prepareStatement(sql)
      for (bind <- bindList) {
        bind(ps)
        ps.addBatch
      }
      ps.executeBatch.toList
    }
  }

  def query[T](sql: String, bind: PreparedStatement => PreparedStatement, extractor: ResultSet => T): List[T] = {
    logger.info(sql)
    withConnection { conn =>
      val ps = bind(conn.prepareStatement(sql))
      val rs = ps.executeQuery
      extractResult(rs, extractor)
    }
  }

  @tailrec
  private def extractResult[T](rs: ResultSet, extractor: ResultSet => T, acc: List[T] = List()): List[T] =
    if (rs.next)
      extractResult(rs, extractor, acc :+ extractor(rs))
    else
      acc
}
