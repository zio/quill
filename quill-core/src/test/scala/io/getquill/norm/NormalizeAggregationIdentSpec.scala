package io.getquill.norm

import io.getquill.base.Spec
import io.getquill.MirrorContexts.testContext._

class NormalizeAggregationIdentSpec extends Spec {
  "multiple select" in {
    val q = quote {
      qr1.groupBy(p => p.i).map { case (i, qrs) =>
        i -> qrs.map(_.l).sum
      }
    }
    val n = quote {
      qr1.groupBy(p => p.i).map { p =>
        p._1 -> p._2.map(x1 => x1.l).sum
      }
    }
    new Normalize(NormalizeCaches.noCache, TranspileConfig.Empty)(q.ast) mustEqual (n.ast)
  }
}
