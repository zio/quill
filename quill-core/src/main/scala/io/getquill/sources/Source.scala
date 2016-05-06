package io.getquill.sources

import scala.reflect.ClassTag
import scala.util.DynamicVariable
import java.io.Closeable

abstract class Source[R: ClassTag, S: ClassTag] extends Closeable {

  type Decoder[T] = io.getquill.sources.Decoder[R, T]
  type Encoder[T] = io.getquill.sources.Encoder[S, T]

  implicit def mappedDecoder[I, O](implicit mapped: MappedEncoding[I, O], decoder: Decoder[I]): Decoder[O] =
    (index: Int, row: R) => mapped.f(decoder(index, row))

  implicit def mappedEncoder[I, O](implicit mapped: MappedEncoding[I, O], encoder: Encoder[O]): Encoder[I] =
    (index: Int, value: I, row: S) => encoder(index, mapped.f(value), row)

  implicit def wrappedTypeDecoder[T <: WrappedType] =
    MappedEncoding[T, T#Type](_.value)

  protected def handleSingleResult[T](list: List[T]) =
    list match {
      case value :: Nil => value
      case other        => throw new IllegalStateException(s"Expected a single result but got $other")
    }
}

object Source {

  private[getquill] val configPrefix = new DynamicVariable[Option[String]](None)
}
