package io.getquill.norm

import io.getquill.base.Spec
import io.getquill.MirrorContexts.testContext.implicitOrd
import io.getquill.MirrorContexts.testContext.qr1
import io.getquill.MirrorContexts.testContext.qr2
import io.getquill.MirrorContexts.testContext.quote
import io.getquill.MirrorContexts.testContext.unquote
import io.getquill.StatelessCacheOpt
import io.getquill.util.TraceConfig

class OrderTermsSpec extends Spec {

  val OrderTerms = new OrderTerms(StatelessCacheOpt.NoCache, TraceConfig.Empty)

  "doesn't reorder groupBy.map" in {
    val q = quote {
      qr1.map(b => b.s).sortBy(b => b)
    }
    OrderTerms.unapply(q.ast) mustEqual None
  }

  "sortBy" - {
    "a.sortBy(b => c).filter(d => e)" in {
      val q = quote {
        qr1.sortBy(b => b.s).filter(d => d.s == "s1")
      }
      val n = quote {
        qr1.filter(d => d.s == "s1").sortBy(b => b.s)
      }
      OrderTerms.unapply(q.ast) mustEqual Some(n.ast)
    }
  }

  "a.flatMap(b => c).?.map(d => e)" - {
    "take" in {
      val q = quote {
        qr1.flatMap(b => qr2).take(3).map(d => d.s)
      }
      val n = quote {
        qr1.flatMap(b => qr2).map(d => d.s).take(3)
      }
      OrderTerms.unapply(q.ast) mustEqual Some(n.ast)
    }
    "drop" in {
      val q = quote {
        qr1.flatMap(b => qr2).drop(3).map(d => d.s)
      }
      val n = quote {
        qr1.flatMap(b => qr2).map(d => d.s).drop(3)
      }
      OrderTerms.unapply(q.ast) mustEqual Some(n.ast)
    }
  }

}
