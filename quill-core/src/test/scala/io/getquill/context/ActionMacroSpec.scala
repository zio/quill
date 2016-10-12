package io.getquill.context

import io.getquill.Spec
import io.getquill.testContext
import io.getquill.testContext._
import io.getquill.context.mirror.Row

class ActionMacroSpec extends Spec {

  "runs non-batched action" - {
    "normal" in {
      val q = quote {
        qr1.delete
      }
      val r = testContext.run(q)
      r.string mustEqual """querySchema("TestEntity").delete"""
      r.prepareRow mustEqual Row()
    }
    "scalar lifting" in {
      val q = quote {
        qr1.insert(t => t.i -> lift(1))
      }
      val r = testContext.run(q)
      r.string mustEqual """querySchema("TestEntity").insert(t => t.i -> ?)"""
      r.prepareRow mustEqual Row(1)
    }
    "case class lifting" in {
      val q = quote {
        qr1.insert(lift(TestEntity("s", 1, 2L, None)))
      }
      val r = testContext.run(q)
      r.string mustEqual """querySchema("TestEntity").insert(v => v.s -> ?, v => v.i -> ?, v => v.l -> ?, v => v.o -> ?)"""
      r.prepareRow mustEqual Row("s", 1, 2L, None)
    }
    "nexted case class lifting" in {
      val q = quote {
        (t: TestEntity) => qr1.insert(t)
      }
      val r = testContext.run(q(lift(TestEntity("s", 1, 2L, None))))
      r.string mustEqual """querySchema("TestEntity").insert(v => v.s -> ?, v => v.i -> ?, v => v.l -> ?, v => v.o -> ?)"""
      r.prepareRow mustEqual Row("s", 1, 2L, None)
    }
    "returning value" in {
      val q = quote {
        qr1.insert(t => t.i -> 1).returning(t => t.l)
      }
      val r = testContext.run(q)
      r.string mustEqual """querySchema("TestEntity").insert(t => t.i -> 1).returning((t) => t.l)"""
      r.prepareRow mustEqual Row()
      r.returningColumn mustEqual "l"
    }
    "scalar lifting + returning value" in {
      val q = quote {
        qr1.insert(t => t.i -> lift(1)).returning(t => t.l)
      }
      val r = testContext.run(q)
      r.string mustEqual """querySchema("TestEntity").insert(t => t.i -> ?).returning((t) => t.l)"""
      r.prepareRow mustEqual Row(1)
      r.returningColumn mustEqual "l"
    }
    "case class lifting + returning value" in {
      val q = quote {
        qr1.insert(lift(TestEntity("s", 1, 2L, None))).returning(t => t.l)
      }
      val r = testContext.run(q)
      r.string mustEqual """querySchema("TestEntity").insert(v => v.s -> ?, v => v.i -> ?, v => v.o -> ?).returning((t) => t.l)"""
      r.prepareRow mustEqual Row("s", 1, None)
      r.returningColumn mustEqual "l"
    }
  }

  "runs batched action" - {

    val entities = List(
      TestEntity("s1", 2, 3L, Some(4)),
      TestEntity("s5", 6, 7L, Some(8))
    )

    "scalar" in {
      val insert = quote {
        (p: Int) => qr1.insert(t => t.i -> p)
      }
      val q = quote {
        liftQuery(List(1, 2)).foreach((p: Int) => insert(p))
      }
      val r = testContext.run(q)
      r.groups mustEqual List(
        """querySchema("TestEntity").insert(t => t.i -> ?)""" -> List(Row(1), Row(2))
      )
    }
    "case class" in {
      val q = quote {
        liftQuery(entities).foreach(p => qr1.insert(p))
      }
      val r = testContext.run(q)
      r.groups mustEqual List(
        """querySchema("TestEntity").insert(v => v.s -> ?, v => v.i -> ?, v => v.l -> ?, v => v.o -> ?)""" ->
          List(Row("s1", 2, 3L, Some(4)), Row("s5", 6, 7L, Some(8)))
      )
    }
    "case class + nested action" in {
      val nested = quote {
        (p: TestEntity) => qr1.insert(p)
      }
      val q = quote {
        liftQuery(entities).foreach(p => nested(p))
      }
      val r = testContext.run(q)
      r.groups mustEqual List(
        """querySchema("TestEntity").insert(v => v.s -> ?, v => v.i -> ?, v => v.l -> ?, v => v.o -> ?)""" ->
          List(Row("s1", 2, 3L, Some(4)), Row("s5", 6, 7L, Some(8)))
      )
    }
    "tuple + case class + nested action" in {
      val nested = quote {
        (s: String, p: TestEntity) => qr1.filter(t => t.s == s).update(p)
      }
      val q = quote {
        liftQuery(entities).foreach(p => nested(lift("s"), p))
      }
      val r = testContext.run(q)
      r.groups mustEqual List(
        """querySchema("TestEntity").filter(t => t.s == ?).update(v => v.s -> ?, v => v.i -> ?, v => v.l -> ?, v => v.o -> ?)""" ->
          List(Row("s", "s1", 2, 3L, Some(4)), Row("s", "s5", 6, 7L, Some(8)))
      )
    }
    "zipWithIndex" in {
      val nested = quote {
        (e: TestEntity, i: Int) => qr1.filter(t => t.i == i).update(e)
      }
      val q = quote {
        liftQuery(entities.zipWithIndex).foreach(p => nested(p._1, p._2))
      }
      val r = testContext.run(q)
      r.groups mustEqual List(
        """querySchema("TestEntity").filter(t => t.i == ?).update(v => v.s -> ?, v => v.i -> ?, v => v.l -> ?, v => v.o -> ?)""" ->
          List(Row(0, "s1", 2, 3, Some(4)), Row(1, "s5", 6, 7, Some(8)))
      )
    }
    "scalar + returning" in {
      val insert = quote {
        (p: Int) => qr1.insert(t => t.i -> p).returning(t => t.l)
      }
      val q = quote {
        liftQuery(List(1, 2)).foreach((p: Int) => insert(p))
      }
      val r = testContext.run(q)
      r.groups mustEqual List(
        ("""querySchema("TestEntity").insert(t => t.i -> ?).returning((t) => t.l)""", "l", List(Row(1), Row(2)))
      )
    }
    "case class + returning" in {
      val q = quote {
        liftQuery(entities).foreach(p => qr1.insert(p).returning(t => t.l))
      }
      val r = testContext.run(q)
      r.groups mustEqual List(
        ("""querySchema("TestEntity").insert(v => v.s -> ?, v => v.i -> ?, v => v.o -> ?).returning((t) => t.l)""", "l",
          List(Row("s1", 2, Some(4)), Row("s5", 6, Some(8))))
      )
    }
    "case class + returning + nested action" in {
      val insert = quote {
        (p: TestEntity) => qr1.insert(p).returning(t => t.l)
      }
      val r = testContext.run(liftQuery(entities).foreach(p => insert(p)))
      r.groups mustEqual List(
        ("""querySchema("TestEntity").insert(v => v.s -> ?, v => v.i -> ?, v => v.o -> ?).returning((t) => t.l)""", "l",
          List(Row("s1", 2, Some(4)), Row("s5", 6, Some(8))))
      )
    }
  }
}
