package io.getquill.source.sql.idiom

import java.util.concurrent.atomic.AtomicInteger

trait H2Dialect
    extends SqlIdiom
    with NullsOrderingClause {

  private[idiom] val preparedStatementId = new AtomicInteger

  override def prepare(sql: String) = 
    s"PREPARE p${preparedStatementId.incrementAndGet} AS ${positionalVariables(sql)}"

  private def positionalVariables(sql: String) =
    sql.foldLeft((1, "")) {
      case ((idx, s), '?') =>
        (idx + 1, s + "$" + idx)
      case ((idx, s), c) =>
        (idx, s + c)
    }._2
}

object H2Dialect extends H2Dialect