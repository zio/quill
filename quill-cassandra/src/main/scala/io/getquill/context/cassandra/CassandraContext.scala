package io.getquill.context.cassandra

import java.util.{ Date, UUID }

import io.getquill.NamingStrategy
import io.getquill.context.Context
import io.getquill.context.cassandra.encoding.{ CassandraMapper, CassandraMapperConversions, CassandraTypes }

import scala.reflect.ClassTag

trait CassandraContext[N <: NamingStrategy]
  extends Context[CqlIdiom, N]
  with CassandraMapperConversions
  with CassandraTypes
  with Ops {

  implicit def optionDecoder[T](implicit d: Decoder[T]): Decoder[Option[T]]
  implicit def optionEncoder[T](implicit d: Encoder[T]): Encoder[Option[T]]

  implicit val stringDecoder: Decoder[String]
  implicit val bigDecimalDecoder: Decoder[BigDecimal]
  implicit val booleanDecoder: Decoder[Boolean]
  implicit val intDecoder: Decoder[Int]
  implicit val longDecoder: Decoder[Long]
  implicit val floatDecoder: Decoder[Float]
  implicit val doubleDecoder: Decoder[Double]
  implicit val byteArrayDecoder: Decoder[Array[Byte]]
  implicit val uuidDecoder: Decoder[UUID]
  implicit val dateDecoder: Decoder[Date]

  implicit val stringEncoder: Encoder[String]
  implicit val bigDecimalEncoder: Encoder[BigDecimal]
  implicit val booleanEncoder: Encoder[Boolean]
  implicit val intEncoder: Encoder[Int]
  implicit val longEncoder: Encoder[Long]
  implicit val floatEncoder: Encoder[Float]
  implicit val doubleEncoder: Encoder[Double]
  implicit val byteArrayEncoder: Encoder[Array[Byte]]
  implicit val uuidEncoder: Encoder[UUID]
  implicit val dateEncoder: Encoder[Date]

  implicit def listDecoder[T, Cas: ClassTag](implicit mapper: CassandraMapper[Cas, T]): Decoder[List[T]]
  implicit def setDecoder[T, Cas: ClassTag](implicit mapper: CassandraMapper[Cas, T]): Decoder[Set[T]]
  implicit def mapDecoder[K, V, KCas: ClassTag, VCas: ClassTag](
    implicit
    keyMapper: CassandraMapper[KCas, K],
    valMapper: CassandraMapper[VCas, V]
  ): Decoder[Map[K, V]]

  implicit def listEncoder[T, Cas](implicit mapper: CassandraMapper[T, Cas]): Encoder[List[T]]
  implicit def setEncoder[T, Cas](implicit mapper: CassandraMapper[T, Cas]): Encoder[Set[T]]
  implicit def mapEncoder[K, V, KCas, VCas](
    implicit
    keyMapper: CassandraMapper[K, KCas],
    valMapper: CassandraMapper[V, VCas]
  ): Encoder[Map[K, V]]
}