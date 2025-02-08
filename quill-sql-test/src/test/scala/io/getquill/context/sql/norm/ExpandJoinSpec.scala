package io.getquill.context.sql.norm

import io.getquill.StatelessCache
import io.getquill.base.Spec
import io.getquill.context.sql.testContext.qr1
import io.getquill.context.sql.testContext.qr2
import io.getquill.context.sql.testContext.qr3
import io.getquill.context.sql.testContext.quote
import io.getquill.context.sql.testContext.unquote
import io.getquill.norm.{Normalize, NormalizeCaches, TranspileConfig}

class ExpandJoinSpec extends Spec {

  val ExpandJoin = new ExpandJoin(StatelessCache.NoCache, new Normalize(NormalizeCaches.noCache, TranspileConfig.Empty))

  "expands the outer join by mapping the result" - {
    "simple" in {
      val q = quote {
        qr1.leftJoin(qr2).on((a, b) => a.s == b.s)
      }
      ExpandJoin(q.ast).toString mustEqual
        """querySchema("TestEntity").leftJoin(querySchema("TestEntity2")).on((a, b) => a.s == b.s).map(ab => (a, b))"""
    }
    "nested" - {
      "inner" in {
        val q = quote {
          qr1.join(qr2).on((a, b) => a.s == b.s).join(qr3).on((c, d) => c._1.s == d.s)
        }
        ExpandJoin(q.ast).toString mustEqual
          """querySchema("TestEntity").join(querySchema("TestEntity2")).on((a, b) => a.s == b.s).join(querySchema("TestEntity3")).on((c, d) => a.s == d.s).map(cd => ((a, b), d))"""
      }
      "left" in {
        val q = quote {
          qr1.leftJoin(qr2).on((a, b) => a.s == b.s).leftJoin(qr3).on((c, d) => c._1.s == d.s)
        }
        ExpandJoin(q.ast).toString mustEqual
          """querySchema("TestEntity").leftJoin(querySchema("TestEntity2")).on((a, b) => a.s == b.s).leftJoin(querySchema("TestEntity3")).on((c, d) => a.s == d.s).map(cd => ((a, b), d))"""
      }
      "right" in {
        val q = quote {
          qr1.leftJoin(qr2.leftJoin(qr3).on((a, b) => a.s == b.s)).on((c, d) => c.s == d._1.s)
        }
        ExpandJoin(q.ast).toString mustEqual
          """querySchema("TestEntity").leftJoin(querySchema("TestEntity2").leftJoin(querySchema("TestEntity3")).on((a, b) => a.s == b.s)).on((c, d) => c.s == a.s).map(cd => (c, (a, b)))"""
      }
      "both" in {
        val q = quote {
          qr1
            .leftJoin(qr2)
            .on((a, b) => a.s == b.s)
            .leftJoin(qr3.leftJoin(qr2).on((c, d) => c.s == d.s))
            .on((e, f) => e._1.s == f._1.s)
        }
        ExpandJoin(q.ast).toString mustEqual
          """querySchema("TestEntity").leftJoin(querySchema("TestEntity2")).on((a, b) => a.s == b.s).leftJoin(querySchema("TestEntity3").leftJoin(querySchema("TestEntity2")).on((c, d) => c.s == d.s)).on((e, f) => a.s == c.s).map(ef => ((a, b), (c, d)))"""
      }
    }
  }
}
