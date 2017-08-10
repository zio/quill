package io.getquill.context.sql.idiom

import io.getquill.Spec
import io.getquill.Literal
import io.getquill.MySQLDialect
import io.getquill.SqlMirrorContext
import io.getquill.TestEntities
import io.getquill.idiom.StringToken

class MySQLDialectSpec extends Spec {

  val ctx = new SqlMirrorContext[MySQLDialect, Literal] with TestEntities
  import ctx._

  "mixes the workaround for offset without limit" in {
    MySQLDialect.isInstanceOf[OffsetWithoutLimitWorkaround] mustEqual true
  }

  "uses CONCAT instead of ||" in {
    val q = quote {
      qr1.map(t => t.s + t.s)
    }
    ctx.run(q).string mustEqual
      "SELECT CONCAT(t.s, t.s) FROM TestEntity t"
  }

  "supports the `prepare` statement" in {
    val sql = s"test"
    MySQLDialect.prepareForProbing(sql) mustEqual
      s"PREPARE p${StringToken(sql.hashCode.abs.toString)} FROM '$sql'"
  }

  "workaround missing nulls ordering feature in mysql" - {
    "asc" in {
      val q = quote {
        qr1.sortBy(t => t.s)(Ord.asc)
      }
      ctx.run(q).string mustEqual
        "SELECT t.s, t.i, t.l, t.o FROM TestEntity t ORDER BY t.s ASC"
    }
    "desc" in {
      val q = quote {
        qr1.sortBy(t => t.s)(Ord.desc)
      }
      ctx.run(q).string mustEqual
        "SELECT t.s, t.i, t.l, t.o FROM TestEntity t ORDER BY t.s DESC"
    }
    "ascNullsFirst" in {
      val q = quote {
        qr1.sortBy(t => t.s)(Ord.ascNullsFirst)
      }
      ctx.run(q).string mustEqual
        "SELECT t.s, t.i, t.l, t.o FROM TestEntity t ORDER BY t.s ASC"
    }
    "descNullsFirst" in {
      val q = quote {
        qr1.sortBy(t => t.s)(Ord.descNullsFirst)
      }
      ctx.run(q).string mustEqual
        "SELECT t.s, t.i, t.l, t.o FROM TestEntity t ORDER BY ISNULL(t.s) DESC, t.s DESC"
    }
    "ascNullsLast" in {
      val q = quote {
        qr1.sortBy(t => t.s)(Ord.ascNullsLast)
      }
      ctx.run(q).string mustEqual
        "SELECT t.s, t.i, t.l, t.o FROM TestEntity t ORDER BY ISNULL(t.s) ASC, t.s ASC"
    }
    "descNullsLast" in {
      val q = quote {
        qr1.sortBy(t => t.s)(Ord.descNullsLast)
      }
      ctx.run(q).string mustEqual
        "SELECT t.s, t.i, t.l, t.o FROM TestEntity t ORDER BY t.s DESC"
    }
  }
  "upserts" - {
    "upsert" in {
      val e = TestEntity("", 1, 1L, Some(1))
      val q = quote {
        query[TestEntity]
          .upsert(lift(e))
          .conflict(_.i)
          .conflictUpdate(_.i -> lift(1), _.l -> lift(1L), _.s -> lift("Test String"))
      }

      val q2 = quote {
        query[TestEntity]
          .upsert(lift(e))
          .conflict(_.i)
          .conflictUpdate(_.l -> 1L)
      }
      val q3 = quote {
        query[TestEntity]
          .upsert(_.s -> "Hi", _.l -> 10L)
          .conflict(_.i)
          .conflictUpdate(_.s -> "Hihi")
      }

      println(ctx.run(q).string)
      println(ctx.run(q2).string)
      println(ctx.run(q3).string)
      /*
      INSERT INTO TestEntity (s, l, o) VALUES (?, ?, ?) ON DUPLICATE KEY UPDATE i = ?, l = ?, s = ?
      INSERT INTO TestEntity (s, l, o) VALUES (?, ?, ?) ON DUPLICATE KEY UPDATE l = 1
      INSERT INTO TestEntity (s, l) VALUES ('Hi', 10) ON DUPLICATE KEY UPDATE s = 'Hihi'
      */
    }
  }
}
