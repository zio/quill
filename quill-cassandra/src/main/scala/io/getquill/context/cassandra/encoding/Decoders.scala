package io.getquill.context.cassandra.encoding

import com.datastax.driver.core.LocalDate
import java.util.{ Date, UUID }

import io.getquill.context.cassandra.CassandraSessionContext
import io.getquill.util.Messages.fail

trait Decoders {
  this: CassandraSessionContext[_] =>

  type Decoder[T] = CassandraDecoder[T]

  case class CassandraDecoder[T](decoder: BaseDecoder[T]) extends BaseDecoder[T] {
    override def apply(index: Index, row: ResultRow) =
      decoder(index, row)
  }

  def decoder[T](d: BaseDecoder[T]): Decoder[T] = CassandraDecoder(
    (index, row) => {
      row.isNull(index) match {
        case true  => fail(s"Expected column at index $index to be defined but is was empty")
        case false => d(index, row)
      }
    }
  )

  def decoder[T](f: ResultRow => Index => T): Decoder[T] =
    decoder((index, row) => f(row)(index))

  implicit def optionDecoder[T](implicit d: Decoder[T]): Decoder[Option[T]] =
    CassandraDecoder((index, row) => {
      row.isNull(index) match {
        case true  => None
        case false => Some(d(index, row))
      }
    })

  implicit def mappedDecoder[I, O](implicit mapped: MappedEncoding[I, O], decoder: Decoder[I]): Decoder[O] =
    CassandraDecoder(mappedBaseDecoder(mapped, decoder.decoder))

  implicit val stringDecoder: Decoder[String] = decoder(_.getString)
  implicit val bigDecimalDecoder: Decoder[BigDecimal] =
    decoder((index, row) => row.getDecimal(index))
  implicit val booleanDecoder: Decoder[Boolean] = decoder(_.getBool)
  implicit val intDecoder: Decoder[Int] = decoder(_.getInt)
  implicit val longDecoder: Decoder[Long] = decoder(_.getLong)
  implicit val floatDecoder: Decoder[Float] = decoder(_.getFloat)
  implicit val doubleDecoder: Decoder[Double] = decoder(_.getDouble)
  implicit val byteArrayDecoder: Decoder[Array[Byte]] =
    decoder((index, row) => {
      val bb = row.getBytes(index)
      val b = new Array[Byte](bb.remaining())
      bb.get(b)
      b
    })
  implicit val uuidDecoder: Decoder[UUID] = decoder(_.getUUID)
  implicit val dateDecoder: Decoder[Date] = decoder(_.getTimestamp)
  implicit val localDateDecoder: Decoder[LocalDate] = decoder(_.getDate)
}
