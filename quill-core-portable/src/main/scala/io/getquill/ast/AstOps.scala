package io.getquill.ast

// Represents an Ident without a Quat
final case class IdentName(name: String)

object Implicits {
  implicit class IdentOps(id: Ident) {
    def idName: IdentName = IdentName(id.name)
  }

  implicit class AstOpsExt(body: Ast) {
    def +||+(other: Ast) = BinaryOperation(body, BooleanOperator.`||`, other)
    def +&&+(other: Ast) = BinaryOperation(body, BooleanOperator.`&&`, other)
    def +==+(other: Ast) = BinaryOperation(body, EqualityOperator.`==`, other)
    def +!=+(other: Ast) = BinaryOperation(body, EqualityOperator.`!=`, other)
  }
}

object +||+ {
  def unapply(a: Ast): Option[(Ast, Ast)] = {
    a match {
      case BinaryOperation(one, BooleanOperator.`||`, two) => Some((one, two))
      case _ => None
    }
  }
}

object +&&+ {
  def unapply(a: Ast): Option[(Ast, Ast)] = {
    a match {
      case BinaryOperation(one, BooleanOperator.`&&`, two) => Some((one, two))
      case _ => None
    }
  }
}

object +==+ {
  def unapply(a: Ast): Option[(Ast, Ast)] = {
    a match {
      case BinaryOperation(one, EqualityOperator.`==`, two) => Some((one, two))
      case _ => None
    }
  }
}

object +!=+ {
  def unapply(a: Ast): Option[(Ast, Ast)] = {
    a match {
      case BinaryOperation(one, EqualityOperator.`!=`, two) => Some((one, two))
      case _ => None
    }
  }
}

object IsNotNullCheck {
  def apply(ast: Ast): BinaryOperation = BinaryOperation(ast, EqualityOperator.`!=`, NullValue)

  def unapply(ast: Ast): Option[Ast] = {
    ast match {
      case BinaryOperation(cond, EqualityOperator.`!=`, NullValue) => Some(cond)
      case _ => None
    }
  }
}

object IsNullCheck {
  def apply(ast: Ast): BinaryOperation = BinaryOperation(ast, EqualityOperator.`==`, NullValue)

  def unapply(ast: Ast): Option[Ast] = {
    ast match {
      case BinaryOperation(cond, EqualityOperator.`==`, NullValue) => Some(cond)
      case _ => None
    }
  }
}

object IfExistElseNull {
  def apply(exists: Ast, `then`: Ast): If =
    If(IsNotNullCheck(exists), `then`, NullValue)

  def unapply(ast: Ast): Option[(Ast, Ast)] = ast match {
    case If(IsNotNullCheck(exists), t, NullValue) => Some((exists, t))
    case _                                        => None
  }
}

object IfExist {
  def apply(exists: Ast, `then`: Ast, otherwise: Ast): If =
    If(IsNotNullCheck(exists), `then`, otherwise)

  def unapply(ast: Ast): Option[(Ast, Ast, Ast)] = ast match {
    case If(IsNotNullCheck(exists), t, e) => Some((exists, t, e))
    case _                                => None
  }
}

object PropertyOrCore {
  def unapply(ast: Ast): Boolean =
    Core.unapply(ast) || ast.isInstanceOf[Property]
}

/* Things that can be on the inside of a series of nested properties */
object Core {
  def unapply(ast: Ast): Boolean =
    ast.isInstanceOf[Ident] || ast.isInstanceOf[Infix] || ast.isInstanceOf[Constant]
}
