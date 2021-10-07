package io.getquill.context

import io.getquill.ast._
import io.getquill.idiom.Statement
import io.getquill.idiom.ReifyStatement
import io.getquill.NamingStrategy
import io.getquill.idiom.Idiom

case class Expand[C <: Context[_, _]](
  val context: C,
  val ast:     Ast,
  statement:   Statement,
  idiom:       Idiom,
  naming:      NamingStrategy
) {

  val (string, externals) =
    ReifyStatement(
      idiom.liftingPlaceholder,
      idiom.emptySetContainsToken,
      statement,
      forProbing = false
    )

  val liftings = externals.collect {
    case lift: ScalarLift => lift
  }

  val prepare =
    (row: context.PrepareRow, session: context.Session) => {
      val (_, values, prepare) = liftings.foldLeft((0, List.empty[Any], row)) {
        case ((idx, values, row), lift) =>
          val encoder = lift.encoder.asInstanceOf[context.Encoder[Any]]
          val newRow = encoder(idx, lift.value, row, session)
          (idx + 1, lift.value :: values, newRow)
      }
      (values, prepare)
    }
}
