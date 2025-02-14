package io.getquill.norm.capture

import io.getquill.MirrorContexts.testContext._
import io.getquill.Query
import io.getquill.base.Spec
import io.getquill.norm.{Normalize, NormalizeCaches, TranspileConfig}

class AvoidAliasConflictComplexSpec extends Spec {

  val normalize = new Normalize(NormalizeCaches.noCache, TranspileConfig.Empty)

  "properly aliases explicit join sets" - {
    import io.getquill.norm.Normalize
    import scala.language.reflectiveCalls

    case class Person(id: Int, name: String)
    case class Address(id: Int, ownerFk: Int, street: String)
    case class Room(addressId: Int, stuff: String)

    "in tail clause" in {
      def fun[T <: { def id: Int }] = quote { (tbl: Query[T]) =>
        for {
          t <- tbl
          a <- query[Address].join(a => a.ownerFk == t.id)
        } yield (t, a)
      }

      def funExpect[T <: { def id: Int }] = quote { (tbl: Query[T]) =>
        for {
          t <- tbl
          a <- query[Address].join(a1 => a1.ownerFk == t.id)
        } yield (t, a)
      }

      val q = quote {
        fun[Person](query[Person].filter(a => a.name == "Joe"))
      }
      val expect = quote {
        funExpect[Person](query[Person].filter(a => a.name == "Joe"))
      }
      normalize(q.ast) mustEqual normalize(expect.ast)
    }

    "in middle clause" in {
      def fun[T <: { def id: Int }] = quote { (tbl: Query[T]) =>
        for {
          t <- tbl
          a <- query[Address].join(a => a.ownerFk == t.id)
          r <- query[Room].join(r => r.addressId == a.id)
        } yield (t, a, r)
      }

      def funExpect[T <: { def id: Int }] = quote { (tbl: Query[T]) =>
        for {
          t <- tbl
          a <- query[Address].join(a1 => a1.ownerFk == t.id)
          r <- query[Room].join(r => r.addressId == a.id)
        } yield (t, a, r)
      }

      val q = quote {
        fun[Person](query[Person].filter(a => a.name == "Joe"))
      }
      val expect = quote {
        funExpect[Person](query[Person].filter(a => a.name == "Joe"))
      }
      normalize(q.ast) mustEqual normalize(expect.ast)
    }

    "in middle and end clause" in {
      def fun[T <: { def id: Int }] = quote { (tbl: Query[T]) =>
        for {
          t <- tbl
          a <- query[Address].join(a => a.ownerFk == t.id)
          r <- query[Room].join(a => a.addressId == 1)
        } yield (t, a, r)
      }

      def funExpect[T <: { def id: Int }] = quote { (tbl: Query[T]) =>
        for {
          t <- tbl
          a <- query[Address].join(a1 => a1.ownerFk == t.id)
          r <- query[Room].join(a2 => a2.addressId == 1)
        } yield (t, a, r)
      }

      val q = quote {
        fun[Person](query[Person].filter(a => a.name == "Joe"))
      }
      val expect = quote {
        funExpect[Person](query[Person].filter(a => a.name == "Joe"))
      }
      normalize(q.ast) mustEqual normalize(expect.ast)
    }
  }
}
