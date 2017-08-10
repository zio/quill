package io.getquill

import java.util.concurrent.atomic.AtomicInteger

import io.getquill.ast._
import io.getquill.context.sql.idiom.{ QuestionMarkBindVariables, SqlIdiom }
import io.getquill.idiom.StatementInterpolator._

trait PostgresDialect
  extends SqlIdiom
  with QuestionMarkBindVariables {

  override implicit def operationTokenizer(implicit propertyTokenizer: Tokenizer[Property], strategy: NamingStrategy): Tokenizer[Operation] =
    Tokenizer[Operation] {
      case UnaryOperation(StringOperator.`toLong`, ast) => stmt"${scopedTokenizer(ast)}::bigint"
      case UnaryOperation(StringOperator.`toInt`, ast)  => stmt"${scopedTokenizer(ast)}::integer"
      case operation                                    => super.operationTokenizer.token(operation)
    }

  private[getquill] val preparedStatementId = new AtomicInteger

  override def prepareForProbing(string: String) =
    s"PREPARE p${preparedStatementId.incrementAndGet.toString.token} AS $string"

  override implicit def actionTokenizer(implicit strategy: NamingStrategy): Tokenizer[Action] = {

    implicit def propertyTokenizer: Tokenizer[Property] = Tokenizer[Property] {
      case Property(Property(_, name), "isEmpty")   => stmt"${strategy.column(name).token} IS NULL"
      case Property(Property(_, name), "isDefined") => stmt"${strategy.column(name).token} IS NOT NULL"
      case Property(Property(_, name), "nonEmpty")  => stmt"${strategy.column(name).token} IS NOT NULL"
      case Property(_, name)                        => strategy.column(name).token
    }

    Tokenizer[Action] {
      case Upsert(table: Entity, assignments) =>
        val columns = assignments.map(_.property.token)
        val values = assignments.map(_.value)
        stmt"INSERT INTO ${table.token} (${columns.mkStmt()}) VALUES (${values.map(scopedTokenizer(_)).mkStmt()})"

      case Conflict(action, _, value)          => stmt"${action.token} ON CONFLICT(${value.token})"

      case ConflictUpdate(action, assignments) => stmt"${action.token} DO UPDATE SET ${assignments.mkStmt()}"

      case Nothing(query)                      => stmt"${query.token} DO NOTHING"

      case action                              => super.actionTokenizer.token(action)
    }
  }
}

object PostgresDialect extends PostgresDialect
