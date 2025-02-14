package io.getquill.norm.capture

import io.getquill.base.Spec
import io.getquill.MirrorContexts.testContext.implicitOrd
import io.getquill.MirrorContexts.testContext.qr1
import io.getquill.MirrorContexts.testContext.qr2
import io.getquill.MirrorContexts.testContext.quote
import io.getquill.MirrorContexts.testContext.unquote
import io.getquill.StatefulCache
import io.getquill.util.TraceConfig

class DealiasSpec extends Spec {

  val Dealias = new DealiasApply(TraceConfig.Empty, StatefulCache.NoCache)

  "ensures that each entity is referenced by the same alias" - {
    "flatMap" in {
      val q = quote {
        qr1.filter(a => a.s == "s").flatMap(b => qr2)
      }
      val n = quote {
        qr1.filter(a => a.s == "s").flatMap(a => qr2)
      }
      Dealias(q.ast) mustEqual n.ast
    }
    "concatMap" in {
      val q = quote {
        qr1.filter(a => a.s == "s").concatMap(b => b.s.split(" "))
      }
      val n = quote {
        qr1.filter(a => a.s == "s").concatMap(a => a.s.split(" "))
      }
      Dealias(q.ast) mustEqual n.ast
    }
    "map" in {
      val q = quote {
        qr1.filter(a => a.s == "s").map(b => b.s)
      }
      val n = quote {
        qr1.filter(a => a.s == "s").map(a => a.s)
      }
      Dealias(q.ast) mustEqual n.ast
    }
    "filter" in {
      val q = quote {
        qr1.filter(a => a.s == "s").filter(b => b.s != "l")
      }
      val n = quote {
        qr1.filter(a => a.s == "s").filter(a => a.s != "l")
      }
      Dealias(q.ast) mustEqual n.ast
    }
    "sortBy" in {
      val q = quote {
        qr1.filter(a => a.s == "s").sortBy(b => b.s)
      }
      val n = quote {
        qr1.filter(a => a.s == "s").sortBy(a => a.s)
      }
      Dealias(q.ast) mustEqual n.ast
    }
    "groupBy" in {
      val q = quote {
        qr1.filter(a => a.s == "s").groupBy(b => b.s)
      }
      val n = quote {
        qr1.filter(a => a.s == "s").groupBy(a => a.s)
      }
      Dealias(q.ast) mustEqual n.ast
    }
    "aggregation" in {
      val q = quote {
        qr1.map(a => a.i).max
      }
      Dealias(q.ast) mustEqual q.ast
    }
    "take" in {
      val q = quote {
        qr1.filter(a => a.s == "s").take(10).map(b => b.s)
      }
      val n = quote {
        qr1.filter(a => a.s == "s").take(10).map(a => a.s)
      }
      Dealias(q.ast) mustEqual n.ast
    }
    "drop" in {
      val q = quote {
        qr1.filter(a => a.s == "s").drop(10).map(b => b.s)
      }
      val n = quote {
        qr1.filter(a => a.s == "s").drop(10).map(a => a.s)
      }
      Dealias(q.ast) mustEqual n.ast
    }
    "union" - {
      "left" in {
        val q = quote {
          qr1.filter(a => a.s == "s").map(b => b.s).union(qr1)
        }
        val n = quote {
          qr1.filter(a => a.s == "s").map(a => a.s).union(qr1)
        }
        Dealias(q.ast) mustEqual n.ast
      }
      "right" in {
        val q = quote {
          qr1.union(qr1.filter(a => a.s == "s").map(b => b.s))
        }
        val n = quote {
          qr1.union(qr1.filter(a => a.s == "s").map(a => a.s))
        }
        Dealias(q.ast) mustEqual n.ast
      }
    }
    "unionAll" - {
      "left" in {
        val q = quote {
          qr1.filter(a => a.s == "s").map(b => b.s).unionAll(qr1)
        }
        val n = quote {
          qr1.filter(a => a.s == "s").map(a => a.s).unionAll(qr1)
        }
        Dealias(q.ast) mustEqual n.ast
      }
      "right" in {
        val q = quote {
          qr1.unionAll(qr1.filter(a => a.s == "s").map(b => b.s))
        }
        val n = quote {
          qr1.unionAll(qr1.filter(a => a.s == "s").map(a => a.s))
        }
        Dealias(q.ast) mustEqual n.ast
      }
    }
    "join" - {
      "left" in {
        val q = quote {
          qr1.filter(a => a.s == "s").map(b => b.s).fullJoin(qr1).on((a, b) => a == b.s)
        }
        val n = quote {
          qr1.filter(a => a.s == "s").map(a => a.s).fullJoin(qr1).on((a, b) => a == b.s)
        }
        Dealias(q.ast) mustEqual n.ast
      }
      "right" in {
        val q = quote {
          qr1.fullJoin(qr1.filter(a => a.s == "s").map(b => b.s)).on((x, y) => x.s == y)
        }
        val n = quote {
          qr1.fullJoin(qr1.filter(a => a.s == "s").map(a => a.s)).on((x, a) => x.s == a)
        }
        Dealias(q.ast) mustEqual n.ast
      }
      "on" in {
        val q = quote {
          qr1.filter(a => a.s == "s").leftJoin(qr1.filter(b => b.s == "s")).on((c, d) => c.s == d.s)
        }
        val n = quote {
          qr1.filter(a => a.s == "s").leftJoin(qr1.filter(b => b.s == "s")).on((a, b) => a.s == b.s)
        }
        Dealias(q.ast) mustEqual n.ast
      }
      "self join" in {
        val q = quote {
          qr1.join(qr1).on((a, b) => a.i == b.i)
        }
        Dealias(q.ast) mustEqual q.ast
      }
    }
    "entity" in {
      Dealias(qr1.ast) mustEqual qr1.ast
    }
    "distinct" in {
      val q = quote {
        qr1.map(a => a.i).distinct
      }
      Dealias(q.ast) mustEqual q.ast
    }
  }
}
