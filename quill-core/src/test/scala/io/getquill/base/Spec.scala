package io.getquill.base

import io.getquill.{BatchActionMirrorGeneric, BatchActionReturningMirrorGeneric}
import io.getquill.ast.{Ident, StatelessTransformer}
import io.getquill.norm.capture.TemporaryIdent
import io.getquill.quat.Quat
import org.scalatest.BeforeAndAfterAll
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import scala.language.higherKinds

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

abstract class Spec extends AnyFreeSpec with Matchers with BeforeAndAfterAll {
  val QV                                = Quat.Value
  val QEP                               = Quat.Product.empty("Product")
  def QP(name: String, fields: String*) = Quat.LeafProduct(name, fields: _*)

  // Used by various tests to replace temporary idents created by AttachToEntity with 'x'
  val replaceTempIdent = new StatelessTransformer {
    override def applyIdent(id: Ident): Ident =
      id match {
        case TemporaryIdent(tid) =>
          Ident("x", id.quat)
        case _ =>
          id
      }
  }

  implicit class QuatOps(quat: Quat) {
    def productOrFail() =
      quat match {
        case p: Quat.Product => p
        case _               => throw new IllegalArgumentException(s"The quat ${quat} is expected to be a product but is not")
      }
  }

  def await[T](f: Future[T]): T = Await.result(f, Duration.Inf)

  implicit class MirrorContextOps(m: BatchActionMirrorGeneric[_]) {
    def triple = {
      if (m.groups.length != 1)
        fail(s"Expected all batch groups per design to only have one root element but has multiple ${m.groups}")
      val (queryString, prepares) = m.groups(0)
      (
        queryString,
        prepares.map { prep =>
          // being explicit here about the fact that this is done per prepare element i.e. all of them are supposed to be Row instances
          prep match {
            case r: io.getquill.context.mirror.Row =>
              r.data
          }
        },
        m.info.executionType
      )
    }

    def tripleBatchMulti =
      m.groups.map { case (queryString, prepares) =>
        (
          queryString,
          prepares.map { prep =>
            // being explicit here about the fact that this is done per prepare element i.e. all of them are supposed to be Row instances
            prep match {
              case r: io.getquill.context.mirror.Row =>
                r.data
            }
          },
          m.info.executionType
        )
      }
  }

  implicit class MirrorReturningContextOps[E[_]](m: BatchActionReturningMirrorGeneric[_, _, E]) {
    def triple = {
      if (m.groups.length != 1)
        fail(s"Expected all batch groups per design to only have one root element but has multiple ${m.groups}")
      val (queryString, returnAction, prepares) = m.groups(0)
      (
        queryString,
        prepares.map { prep =>
          // being explicit here about the fact that this is done per prepare element i.e. all of them are supposed to be Row instances
          prep match {
            case r: io.getquill.context.mirror.Row =>
              r.data
          }
        },
        m.info.executionType
      )
    }

    def tripleBatchMulti =
      m.groups.map { case (queryString, returnAction, prepares) =>
        (
          queryString,
          prepares.map { prep =>
            // being explicit here about the fact that this is done per prepare element i.e. all of them are supposed to be Row instances
            prep match {
              case r: io.getquill.context.mirror.Row =>
                r.data
            }
          },
          m.info.executionType
        )
      }
  }

}
