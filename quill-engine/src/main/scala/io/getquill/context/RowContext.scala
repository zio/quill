package io.getquill.context

import io.getquill.ReturnAction
import io.getquill.ast.{External, ScalarLift}

trait RowContext {
  type PrepareRow
  type ResultRow

  protected val identityPrepare: Prepare           = (p: PrepareRow, _: Session) => (Nil, p)
  private val _identityExtractor: Extractor[Any]   = (rr: ResultRow, _: Session) => rr
  protected def identityExtractor[T]: Extractor[T] = _identityExtractor.asInstanceOf[Extractor[T]]

  // TODO need this for backwards compat
  case class BatchGroup(string: String, prepare: List[Prepare], liftings: List[List[External]])
  case class BatchGroupReturning(string: String, returningBehavior: ReturnAction, prepare: List[Prepare], liftings: List[List[External]])

  type Prepare      = (PrepareRow, Session) => (List[Any], PrepareRow)
  type Extractor[T] = (ResultRow, Session) => T
  type Session
}
