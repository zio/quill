package io.getquill.context

import io.getquill.Spec
import io.getquill.ast.Function
import io.getquill.context.mirror.Row
import io.getquill.testContext
import io.getquill.testContext.lift
import io.getquill.testContext.qr1
import io.getquill.testContext.quote
import io.getquill.testContext.unquote

class QueryMacroSpec extends Spec {

  "runs non-binded query" in {
    val q = quote {
      qr1.map(_.i)
    }
    testContext.run(q).ast mustEqual q.ast
  }

  "runs binded query" - {
    "one param" in {
      val q = quote {
        (p1: Int) => qr1.filter(t => t.i == p1).map(t => t.i)
      }
      val r = testContext.run(q)(1)
      r.ast mustEqual q.ast.body
      r.binds mustEqual Row(1)
    }
    "two params" in {
      val q = quote {
        (p1: Int, p2: String) => qr1.filter(t => t.i == p1 && t.s == p2).map(t => t.i)
      }
      val r = testContext.run(q)(1, "a")
      r.ast mustEqual q.ast.body
      r.binds mustEqual Row(1, "a")
    }
    "in-place param" in {
      val q = quote {
        (p1: Int) => qr1.filter(t => t.i == p1).map(t => t.i)
      }
      val p1 = 1
      val r = testContext.run(q(lift(p1)))
      r.binds mustEqual Row(p1)
    }
    "in-place param and function param" in {
      val q = quote { (i1: Int) => (i2: Int) => qr1.filter(t => t.i == i1).map(t => t.i + i2)
      }
      val v1 = 1
      val v2 = 2
      val r = testContext.run(q(lift(v1)))(v2)

      q.ast.body match {
        case f: Function => r.ast mustEqual r.ast
        case other       => fail
      }
      r.binds mustEqual Row(v1, v2)
    }
    "inline" in {
      def q(i: Int) =
        testContext.run(qr1.filter(_.i == lift(i)))
      q(1).binds mustEqual Row(1)
    }
  }
}
