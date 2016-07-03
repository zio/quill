package io.getquill.context.sql.norm

import io.getquill.Spec
import io.getquill.context.sql.testContext
import io.getquill.context.sql.testContext.qr1
import io.getquill.context.sql.testContext.qr2
import io.getquill.context.sql.testContext.quote
import io.getquill.context.sql.testContext.unquote

class ExpandNestedQueriesSpec extends Spec {

  "keeps the initial table alias" in {
    val q = quote {
      (for {
        a <- qr1
        b <- qr2
      } yield b).take(10)
    }

    testContext.run(q).sql mustEqual
      "SELECT x.s, x.i, x.l, x.o FROM (SELECT x.s, x.i, x.l, x.o FROM TestEntity a, TestEntity2 x) x LIMIT 10"
  }

  "partial select" in {
    val q = quote {
      (for {
        a <- qr1
        b <- qr2
      } yield b.i).take(10)
    }
    testContext.run(q).sql mustEqual
      "SELECT x.* FROM (SELECT b.i FROM TestEntity a, TestEntity2 b) x LIMIT 10"
  }
}
