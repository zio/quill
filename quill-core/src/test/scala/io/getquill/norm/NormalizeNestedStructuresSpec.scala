package io.getquill.norm

import io.getquill.base.Spec
import io.getquill.MirrorContexts.testContext.implicitOrd
import io.getquill.MirrorContexts.testContext.qr1
import io.getquill.MirrorContexts.testContext.qr2
import io.getquill.MirrorContexts.testContext.quote
import io.getquill.MirrorContexts.testContext.unquote
import io.getquill.StatelessCacheOpt

class NormalizeNestedStructuresSpec extends Spec {

  val unnormalized = quote {
    qr1.map(x => x.i).take(1).size
  }

  val normalized = quote {
    qr1.take(1).map(x => x.i).size
  }

  val NormalizeNestedStructures = new NormalizeNestedStructures(new Normalize(NormalizeCaches.noCache, TranspileConfig.Empty), StatelessCacheOpt.NoCache)

  "returns Some if a nested structure changes" - {
    "flatMap" in {
      val q = quote {
        qr1.flatMap(x => qr2.map(y => y.s).filter(s => s == "s"))
      }
      val n = quote {
        qr1.flatMap(x => qr2.filter(y => y.s == "s").map(y => y.s))
      }
      NormalizeNestedStructures.unapply(q.ast) mustEqual Some(n.ast)
    }
    "concatMap" in {
      val q = quote {
        qr1.map(y => y.s).filter(s => s == "s").concatMap(s => s.split(" "))
      }
      val n = quote {
        qr1.filter(y => y.s == "s").map(y => y.s).concatMap(s => s.split(" "))
      }
      NormalizeNestedStructures.unapply(q.ast) mustEqual Some(n.ast)
    }
    "filter" in {
      val q = quote {
        qr1.filter(x => qr2.map(y => y.s).filter(s => s == "s").isEmpty)
      }
      val n = quote {
        qr1.filter(x => qr2.filter(y => y.s == "s").map(y => y.s).isEmpty)
      }
      NormalizeNestedStructures.unapply(q.ast) mustEqual Some(n.ast)
    }
    "map" in {
      val q = quote {
        qr1.map(x => qr2.map(y => y.s).filter(s => s == "s").isEmpty)
      }
      val n = quote {
        qr1.map(x => qr2.filter(y => y.s == "s").map(y => y.s).isEmpty)
      }
      NormalizeNestedStructures.unapply(q.ast) mustEqual Some(n.ast)
    }
    "sortBy" in {
      val q = quote {
        qr1.sortBy(t => unnormalized)
      }
      val n = quote {
        qr1.sortBy(t => normalized)
      }
      NormalizeNestedStructures.unapply(q.ast) mustEqual Some(n.ast)
    }
    "groupBy" in {
      val q = quote {
        qr1.groupBy(t => unnormalized)
      }
      val n = quote {
        qr1.groupBy(t => normalized)
      }
      NormalizeNestedStructures.unapply(q.ast) mustEqual Some(n.ast)
    }
    "aggregation" in {
      val q = quote {
        qr1.map(t => unnormalized).max
      }
      val n = quote {
        qr1.map(t => normalized).max
      }
      NormalizeNestedStructures.unapply(q.ast) mustEqual Some(n.ast)
    }
    "take" in {
      val q = quote {
        qr1.sortBy(t => unnormalized).take(1)
      }
      val n = quote {
        qr1.sortBy(t => normalized).take(1)
      }
      NormalizeNestedStructures.unapply(q.ast) mustEqual Some(n.ast)
    }
    "drop" in {
      val q = quote {
        qr1.sortBy(t => unnormalized).drop(1)
      }
      val n = quote {
        qr1.sortBy(t => normalized).drop(1)
      }
      NormalizeNestedStructures.unapply(q.ast) mustEqual Some(n.ast)
    }
    "union" in {
      val q = quote {
        qr1.filter(t => unnormalized == 1L).union(qr1)
      }
      val n = quote {
        qr1.filter(t => normalized == 1L).union(qr1)
      }
      NormalizeNestedStructures.unapply(q.ast) mustEqual Some(n.ast)
    }
    "unionAll" in {
      val q = quote {
        qr1.filter(t => unnormalized == 1L).unionAll(qr1)
      }
      val n = quote {
        qr1.filter(t => normalized == 1L).unionAll(qr1)
      }
      NormalizeNestedStructures.unapply(q.ast) mustEqual Some(n.ast)
    }
    "outer join" - {
      "left" in {
        val q = quote {
          qr1.filter(t => unnormalized == 1L).rightJoin(qr1).on((a, b) => a.s == b.s)
        }
        val n = quote {
          qr1.filter(t => normalized == 1L).rightJoin(qr1).on((a, b) => a.s == b.s)
        }
        NormalizeNestedStructures.unapply(q.ast) mustEqual Some(n.ast)
      }
      "right" in {
        val q = quote {
          qr1.rightJoin(qr1.filter(t => unnormalized == 1L)).on((a, b) => a.s == b.s)
        }
        val n = quote {
          qr1.rightJoin(qr1.filter(t => normalized == 1L)).on((a, b) => a.s == b.s)
        }
        NormalizeNestedStructures.unapply(q.ast) mustEqual Some(n.ast)
      }
      "on" in {
        val q = quote {
          qr1.rightJoin(qr1).on((a, b) => unnormalized == 1L)
        }
        val n = quote {
          qr1.rightJoin(qr1).on((a, b) => normalized == 1L)
        }
        NormalizeNestedStructures.unapply(q.ast) mustEqual Some(n.ast)
      }
    }
    "distinct" in {
      val q = quote {
        qr1.filter(t => unnormalized == 1L).distinct
      }
      val n = quote {
        qr1.filter(t => normalized == 1L).distinct
      }
      NormalizeNestedStructures.unapply(q.ast) mustEqual Some(n.ast)
    }
  }

  "returns None if none of the nested structures changes" - {
    "flatMap" in {
      val q = quote {
        qr1.flatMap(x => qr2)
      }
      NormalizeNestedStructures.unapply(q.ast) mustEqual None
    }
    "concatMap" in {
      val q = quote {
        qr1.concatMap(x => x.s.split(" "))
      }
      NormalizeNestedStructures.unapply(q.ast) mustEqual None
    }
    "filter" in {
      val q = quote {
        qr1.filter(x => x.i == 1)
      }
      NormalizeNestedStructures.unapply(q.ast) mustEqual None
    }
    "map" in {
      val q = quote {
        qr1.map(x => x.i)
      }
      NormalizeNestedStructures.unapply(q.ast) mustEqual None
    }
    "sortBy" in {
      val q = quote {
        qr1.sortBy(t => t.i)
      }
      NormalizeNestedStructures.unapply(q.ast) mustEqual None
    }
    "groupBy" in {
      val q = quote {
        qr1.groupBy(t => t.i)
      }
      NormalizeNestedStructures.unapply(q.ast) mustEqual None
    }
    "take" in {
      val q = quote {
        qr1.sortBy(t => t.i).take(2)
      }
      NormalizeNestedStructures.unapply(q.ast) mustEqual None
    }
    "drop" in {
      val q = quote {
        qr1.sortBy(t => t.i).drop(2)
      }
      NormalizeNestedStructures.unapply(q.ast) mustEqual None
    }
    "union" in {
      val q = quote {
        qr1.filter(t => t.s == "a").union(qr1)
      }
      NormalizeNestedStructures.unapply(q.ast) mustEqual None
    }
    "unionAll" in {
      val q = quote {
        qr1.filter(t => t.s == "a").unionAll(qr1)
      }
      NormalizeNestedStructures.unapply(q.ast) mustEqual None
    }
    "outer join" in {
      val q = quote {
        qr1.filter(t => t.s == "a").rightJoin(qr1).on(_.s == _.s)
      }
      NormalizeNestedStructures.unapply(q.ast) mustEqual None
    }
  }
}
