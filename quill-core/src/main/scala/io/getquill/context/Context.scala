package io.getquill.context

import scala.language.higherKinds
import scala.reflect.ClassTag

import io.getquill.dsl.CoreDsl
import java.io.Closeable
import io.getquill.WrappedType

abstract class Context[R: ClassTag, S: ClassTag] extends Closeable with CoreDsl {

  type QueryResult[T]
  type SingleQueryResult[T]
  type ActionResult[T, O]
  type BatchedActionResult[T, O]
  type Params[T]

  type Decoder[T] = io.getquill.context.Decoder[R, T]
  type Encoder[T] = io.getquill.context.Encoder[S, T]

  abstract class ActionApply[T, O](f: Params[T] => BatchedActionResult[T, O]) {
    def apply(params: Params[T]): BatchedActionResult[T, O] = f(params)
    def apply(param: T)(implicit dummy: DummyImplicit): ActionResult[T, O]
  }

  def actionApply[T, O](f: Params[T] => BatchedActionResult[T, O]): ActionApply[T, O]

  case class MappedEncoding[I, O](f: I => O)

  def mappedEncoding[I, O](f: I => O) = MappedEncoding(f)

  implicit def mappedDecoder[I, O](implicit mapped: MappedEncoding[I, O], decoder: Decoder[I]): Decoder[O] =
    new Decoder[O] {
      def apply(index: Int, row: R) =
        mapped.f(decoder(index, row))
    }

  implicit def mappedEncoder[I, O](implicit mapped: MappedEncoding[I, O], encoder: Encoder[O]): Encoder[I] =
    new Encoder[I] {
      def apply(index: Int, value: I, row: S) =
        encoder(index, mapped.f(value), row)
    }

  implicit def wrappedTypeDecoder[T <: WrappedType] =
    MappedEncoding[T, T#Type](_.value)

  protected def handleSingleResult[T](list: List[T]) =
    list match {
      case value :: Nil => value
      case other        => throw new IllegalStateException(s"Expected a single result but got $other")
    }
}
