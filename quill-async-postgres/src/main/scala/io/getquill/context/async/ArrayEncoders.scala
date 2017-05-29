package io.getquill.context.async

import java.time.LocalDate
import java.util.Date

import io.getquill.PostgresAsyncContext
import io.getquill.context.sql.encoding.ArrayEncoding
import org.joda.time.{ LocalDate => JodaLocalDate, LocalDateTime => JodaLocalDateTime }

trait ArrayEncoders extends ArrayEncoding {
  self: PostgresAsyncContext[_] =>

  implicit def arrayStringEncoder[Col <: Seq[String]]: Encoder[Col] = arrayRawEncoder[String, Col]
  implicit def arrayBigDecimalEncoder[Col <: Seq[BigDecimal]]: Encoder[Col] = arrayRawEncoder[BigDecimal, Col]
  implicit def arrayBooleanEncoder[Col <: Seq[Boolean]]: Encoder[Col] = arrayRawEncoder[Boolean, Col]
  implicit def arrayByteEncoder[Col <: Seq[Byte]]: Encoder[Col] = arrayRawEncoder[Byte, Col]
  implicit def arrayShortEncoder[Col <: Seq[Short]]: Encoder[Col] = arrayRawEncoder[Short, Col]
  implicit def arrayIntEncoder[Col <: Seq[Index]]: Encoder[Col] = arrayRawEncoder[Index, Col]
  implicit def arrayLongEncoder[Col <: Seq[Long]]: Encoder[Col] = arrayRawEncoder[Long, Col]
  implicit def arrayFloatEncoder[Col <: Seq[Float]]: Encoder[Col] = arrayRawEncoder[Float, Col]
  implicit def arrayDoubleEncoder[Col <: Seq[Double]]: Encoder[Col] = arrayRawEncoder[Double, Col]
  implicit def arrayDateEncoder[Col <: Seq[Date]]: Encoder[Col] = arrayRawEncoder[Date, Col]
  implicit def arrayLocalDateTimeJodaEncoder[Col <: Seq[JodaLocalDateTime]]: Encoder[Col] = arrayRawEncoder[JodaLocalDateTime, Col]
  implicit def arrayLocalDateJodaEncoder[Col <: Seq[JodaLocalDate]]: Encoder[Col] = arrayRawEncoder[JodaLocalDate, Col]
  implicit def arrayLocalDateEncoder[Col <: Seq[LocalDate]]: Encoder[Col] = arrayRawEncoder[LocalDate, Col]

  def arrayEncoder[T, Col <: Seq[T]](mapper: T => Any): Encoder[Col] =
    encoder[Col]((col: Col) => col.toIndexedSeq.map(mapper), SqlTypes.ARRAY)

  def arrayRawEncoder[T, Col <: Seq[T]]: Encoder[Col] = arrayEncoder[T, Col](identity)
}
