package io.getquill.context

import io.getquill.{NamingStrategy, ReturnAction}
import io.getquill.ast.Ast
import io.getquill.quat.Quat

import scala.language.higherKinds

/**
 * A common context used between Quill and ProtoQuill. This is more like a
 * pre-context because the actual `run` methods cannot be contained here since
 * they use macros. Right now not all Scala2-Quill context extend this context
 * but hopefully they will all in the future. This will establish a common
 * general-api that Quill contexts can use. In ProtoQuill, this context is used
 * for the base of all other context and allows the Scala 3 macros to call the
 * `execute___` methods. In Scala2-Quill wherein macros are less strict about
 * signatures, this cannot be used for `Context` (in `Context.scala`) but
 * various higher-level context extend it as a guard-rail against API drift i.e.
 * so that the Scala2-Quill and ProtoQuill internal-context APIs remain largely
 * the same.
 */
trait ProtoContext[+Dialect <: io.getquill.idiom.Idiom, +Naming <: NamingStrategy] extends RowContext {
  type PrepareRow
  type ResultRow

  type Result[T]
  type RunQuerySingleResult[T]
  type RunQueryResult[T]
  type RunActionResult
  type RunActionReturningResult[T]
  type RunBatchActionResult
  type RunBatchActionReturningResult[T]
  type Session

  /** Future class to hold things like ExecutionContext for Cassandra etc... */
  type Runner

  def idiom: Dialect
  def naming: Naming

  def executeQuery[T](sql: String, prepare: Prepare, extractor: Extractor[T])(
    executionInfo: ExecutionInfo,
    rn: Runner
  ): Result[RunQueryResult[T]]
  def executeQuerySingle[T](string: String, prepare: Prepare, extractor: Extractor[T])(
    executionInfo: ExecutionInfo,
    rn: Runner
  ): Result[RunQuerySingleResult[T]]
  def executeAction(sql: String, prepare: Prepare)(executionInfo: ExecutionInfo, rn: Runner): Result[RunActionResult]
  def executeActionReturning[T](
    sql: String,
    prepare: Prepare,
    extractor: Extractor[T],
    returningBehavior: ReturnAction
  )(executionInfo: ExecutionInfo, rn: Runner): Result[RunActionReturningResult[T]]
  def executeBatchAction(
    groups: List[BatchGroup]
  )(executionInfo: ExecutionInfo, rn: Runner): Result[RunBatchActionResult]
  def executeBatchActionReturning[T](groups: List[BatchGroupReturning], extractor: Extractor[T])(
    executionInfo: ExecutionInfo,
    rn: Runner
  ): Result[RunBatchActionReturningResult[T]]
}

/**
 * Metadata related to query execution. Note that AST should be lazy so as not
 * to be evaluated at runtime (which would happen with a by-value property since
 * `{ ExecutionInfo(stuff, ast) } is spliced into a query-execution site).
 * Additionally, there are performance overheads even splicing the finalized
 * version of the AST into call sites of the `run` functions. For this reason,
 * this functionality is being used only in ProtoQuill and only when a trait
 * extends the trait AstSplicing. In the future it might potentially be
 * controlled by a compiler argument.
 */
class ExecutionInfo(val executionType: ExecutionType, queryAst: => Ast, queryTopLevelQuat: => Quat) {
  def ast: Ast           = queryAst
  def topLevelQuat: Quat = queryTopLevelQuat
}
object ExecutionInfo {
  def apply(executionType: ExecutionType, ast: => Ast, topLevelQuat: => Quat) =
    new ExecutionInfo(executionType, ast, topLevelQuat)
  val unknown = ExecutionInfo(ExecutionType.Unknown, io.getquill.ast.NullValue, Quat.Unknown)
}

trait AstSplicing

sealed trait ExecutionType
object ExecutionType {
  case object Dynamic extends ExecutionType
  case object Static  extends ExecutionType
  case object Unknown extends ExecutionType
}

trait ProtoStreamContext[Dialect <: io.getquill.idiom.Idiom, +Naming <: NamingStrategy] extends RowContext {
  type PrepareRow
  type ResultRow

  type Runner
  type StreamResult[T]
  type Session

  def streamQuery[T](fetchSize: Option[Int], sql: String, prepare: Prepare, extractor: Extractor[T])(
    info: ExecutionInfo,
    rn: Runner
  ): StreamResult[T]
}
