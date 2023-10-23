package io.getquill.context

import io.getquill.ast._
import io.getquill.idiom._
import io.getquill.{IdiomContext, NamingStrategy}

object CanDoBatchedInsert {
  private val right: Right[String, Unit] = Right(())

  def apply(ast: Ast, idiom: Idiom, statement: Token, isReturning: Boolean, idiomContext: IdiomContext): Boolean = {
    // find any actions that could have a VALUES clause. Right now just ast.Insert,
    // in the future might be Update and Delete
    val actions = CollectAst.byType[Action](ast)
    // only one action allowed per-query in general
    if (actions.length != 1) false
    else {
      val validations =
        for {
          _ <- validateConcatenatedIterationPossible(statement)
          _ <- validateIdiomSupportsConcatenatedIteration(idiom, isReturning)
        } yield ()

      validations match {
        case Right(_) => true
        case Left(_)  => false
      }
    }
  }

  private def validateIdiomSupportsConcatenatedIteration(idiom: Idiom, doingReturning: Boolean): Either[String, Unit] =
    if (doingReturning) validateIdiomSupportsConcatenatedIterationReturning(idiom)
    else validateIdiomSupportsConcatenatedIterationNormal(idiom)

  private def validateIdiomSupportsConcatenatedIterationNormal(idiom: Idiom): Either[String, Unit] = {
    val hasCapability =
      idiom match {
        case capability: IdiomInsertValueCapability => capability.idiomInsertValuesCapability == InsertValueMulti
        case _                                      => false
      }

    if (hasCapability) right
    else
      Left(
        s"""|The dialect ${idiom.getClass.getName} does not support inserting multiple rows-per-batch (e.g. it cannot support multiple VALUES clauses).
            |Currently this functionality is only supported for INSERT queries for select databases (Postgres, H2, SQL Server, Sqlite).
            |Falling back to the regular single-row-per-batch insert behavior.
            |""".stripMargin
      )
  }

  private def validateIdiomSupportsConcatenatedIterationReturning(idiom: Idiom): Either[String, Unit] = {
    val hasCapability =
      idiom match {
        case capability: IdiomInsertReturningValueCapability =>
          capability.idiomInsertReturningValuesCapability == InsertReturningValueMulti
        case _ => false
      }

    if (hasCapability) right
    else
      Left(
        s"""|The dialect ${idiom.getClass.getName} does not support inserting multiple rows-per-batch (e.g. it cannot support multiple VALUES clauses)
            |when batching with query-returns and/or generated-keys.
            |Currently this functionality is only supported for INSERT queries for select databases (Postgres, H2, SQL Server).
            |Falling back to the regular single-row-per-batch insert-returning behavior.
            |""".stripMargin
      )
  }

  private def validateConcatenatedIterationPossible(realQuery: Token): Either[String, Unit] = {
    import io.getquill.idiom._
    def valueClauseExistsIn(token: Token): Boolean =
      token match {
        case _: ValuesClauseToken           => true
        case _: StringToken                 => false
        case _: ScalarTagToken              => false
        case _: QuotationTagToken           => false
        case _: ScalarLiftToken             => false
        case Statement(tokens: List[Token]) => tokens.exists(valueClauseExistsIn)
        case SetContainsToken(a: Token, op: Token, b: Token) =>
          valueClauseExistsIn(a) || valueClauseExistsIn(op) || valueClauseExistsIn(b)
      }

    if (valueClauseExistsIn(realQuery)) right
    else
      Left(
        s"""|Cannot insert multiple rows per-batch-query since the query has no VALUES clause.
            |Currently this functionality is only supported for INSERT queries for select databases (Postgres, H2, SQL Server, Sqlite).
            |Falling back to the regular single-row-per-batch insert behavior.
            |""".stripMargin
      )
  }
}

final case class Expand[C <: Context[_, _]](
  context: C,
  ast: Ast,
  statement: Statement,
  idiom: Idiom,
  naming: NamingStrategy,
  executionType: ExecutionType
) {

  val (string, externals) =
    ReifyStatement(
      idiom.liftingPlaceholder,
      idiom.emptySetContainsToken,
      statement,
      forProbing = false
    )

  val liftings = externals.collect { case lift: ScalarLift =>
    lift
  }

  val prepare =
    (row: context.PrepareRow, session: context.Session) => {
      val (_, values, prepare) = liftings.foldLeft((0, List.empty[Any], row)) { case ((idx, values, row), lift) =>
        val encoder = lift.encoder.asInstanceOf[context.Encoder[Any]]
        val newRow  = encoder(idx, lift.value, row, session)
        (idx + 1, lift.value :: values, newRow)
      }
      (values, prepare)
    }
}

final case class ExpandWithInjectables[T, C <: Context[_, _]](
  context: C,
  ast: Ast,
  statement: Statement,
  idiom: Idiom,
  naming: NamingStrategy,
  executionType: ExecutionType,
  subBatch: List[T],
  injectables: List[(String, T => ScalarLift)]
) {

  val (string, externals) =
    ReifyStatementWithInjectables(
      idiom.liftingPlaceholder,
      idiom.emptySetContainsToken,
      statement,
      forProbing = false,
      subBatch,
      injectables
    )

  val liftings = externals.collect { case lift: ScalarLift =>
    lift
  }

  val prepare =
    (row: context.PrepareRow, session: context.Session) => {
      val (_, values, prepare) = liftings.foldLeft((0, List.empty[Any], row)) { case ((idx, values, row), lift) =>
        val encoder = lift.encoder.asInstanceOf[context.Encoder[Any]]
        val newRow  = encoder(idx, lift.value, row, session)
        (idx + 1, lift.value :: values, newRow)
      }
      (values, prepare)
    }
}
