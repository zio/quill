package io.getquill.context.mirror

import java.util.Date
import io.getquill.context.Context

trait MirrorEncoders {
  this: Context[Row, Row] =>

  private def encoder[T] = new Encoder[T] {
    def apply(index: Int, value: T, row: Row) =
      row.add(value)
  }

  implicit def optionEncoder[T](implicit d: Encoder[T]): Encoder[Option[T]] =
    new Encoder[Option[T]] {
      def apply(index: Int, value: Option[T], row: Row) =
        row.add(value)
    }

  implicit def traversableEncoder[T](implicit d: Encoder[T]): Encoder[Traversable[T]] =
    new Encoder[Traversable[T]] {
      def apply(index: Int, value: Traversable[T], row: Row) = row.add(value)
    }

  implicit val stringEncoder = encoder[String]
  implicit val bigDecimalEncoder = encoder[BigDecimal]
  implicit val booleanEncoder = encoder[Boolean]
  implicit val byteEncoder = encoder[Byte]
  implicit val shortEncoder = encoder[Short]
  implicit val intEncoder = encoder[Int]
  implicit val longEncoder = encoder[Long]
  implicit val floatEncoder = encoder[Float]
  implicit val doubleEncoder = encoder[Double]
  implicit val byteArrayEncoder = encoder[Array[Byte]]
  implicit val dateEncoder = encoder[Date]
}
