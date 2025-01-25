package io.getquill.norm

import io.getquill.base.Spec
import io.getquill.MirrorContexts.testContext.qr1
import io.getquill.MirrorContexts.testContext.qr2
import io.getquill.MirrorContexts.testContext.quote
import io.getquill.MirrorContexts.testContext.unquote
import io.getquill.{StatelessCache, StatelessCacheOpt}
import io.getquill.util.TraceConfig

class AdHocReductionSpec extends Spec {

  val AdHocReduction = new AdHocReduction(StatelessCacheOpt.NoCache, TraceConfig.Empty)

  "*.filter" - {
    "a.filter(b => c).filter(d => e)" in {
      val q = quote {
        qr1.filter(b => b.s == "s1").filter(d => d.s == "s2")
      }
      val n = quote {
        qr1.filter(b => b.s == "s1" && b.s == "s2")
      }
      AdHocReduction.unapply(q.ast) mustEqual Some(n.ast)
    }
  }

  "flatMap.*" - {
    "a.flatMap(b => c).map(d => e)" in {
      val q = quote {
        qr1.flatMap(b => qr2).map(d => d.s)
      }
      val n = quote {
        qr1.flatMap(b => qr2.map(d => d.s))
      }
      AdHocReduction.unapply(q.ast) mustEqual Some(n.ast)
    }
    "a.flatMap(b => c).filter(d => e)" in {
      val q = quote {
        qr1.flatMap(b => qr2).filter(d => d.s == "s2")
      }
      val n = quote {
        qr1.flatMap(b => qr2.filter(d => d.s == "s2"))
      }
      AdHocReduction.unapply(q.ast) mustEqual Some(n.ast)
    }
    "a.flatMap(b => c.union(d))" in {
      val q = quote {
        qr1.flatMap(b => qr2.filter(t => t.i == 1).union(qr2.filter(t => t.s == "s")))
      }
      val n = quote {
        qr1.flatMap(b => qr2.filter(t => t.i == 1)).union(qr1.flatMap(b => qr2.filter(t => t.s == "s")))
      }
      AdHocReduction.unapply(q.ast) mustEqual Some(n.ast)
    }
    "a.flatMap(b => c.unionAll(d))" in {
      val q = quote {
        qr1.flatMap(b => qr2.filter(t => t.i == 1).unionAll(qr2.filter(t => t.s == "s")))
      }
      val n = quote {
        qr1.flatMap(b => qr2.filter(t => t.i == 1)).unionAll(qr1.flatMap(b => qr2.filter(t => t.s == "s")))
      }
      AdHocReduction.unapply(q.ast) mustEqual Some(n.ast)
    }
  }
}
