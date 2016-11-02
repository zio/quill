package io.getquill.norm.capture

import io.getquill.Spec
import io.getquill.testContext.implicitOrd
import io.getquill.testContext.qr1
import io.getquill.testContext.qr2
import io.getquill.testContext.qr3
import io.getquill.testContext.quote
import io.getquill.testContext.unquote

class AvoidAliasConflictSpec extends Spec {

  "renames alias to avoid conflict between entities during normalization" - {
    "flatMap" in {
      val q = quote {
        qr1.flatMap(a => qr2.flatMap(a => qr3))
      }
      val n = quote {
        qr1.flatMap(a => qr2.flatMap(a1 => qr3))
      }
      AvoidAliasConflict(q.ast) mustEqual n.ast
    }
    "map" in {
      val q = quote {
        qr1.flatMap(a => qr2.map(a => a.s))
      }
      val n = quote {
        qr1.flatMap(a => qr2.map(a1 => a1.s))
      }
      AvoidAliasConflict(q.ast) mustEqual n.ast
    }
    "filter" in {
      val q = quote {
        qr1.flatMap(a => qr2.filter(a => a.i == 1))
      }
      val n = quote {
        qr1.flatMap(a => qr2.filter(a1 => a1.i == 1))
      }
      AvoidAliasConflict(q.ast) mustEqual n.ast
    }
    "sortBy" in {
      val q = quote {
        qr1.flatMap(a => qr2.sortBy(a => a.s))
      }
      val n = quote {
        qr1.flatMap(a => qr2.sortBy(a1 => a1.s))
      }
      AvoidAliasConflict(q.ast) mustEqual n.ast
    }
    "outer join" - {
      "both sides" in {
        val q = quote {
          for {
            a <- qr1
            b <- qr1
            c <- qr1.leftJoin(qr2).on((a, b) => a.s == b.s)
          } yield {
            (a, b, c)
          }
        }
        val n = quote {
          for {
            a <- qr1
            b <- qr1
            c <- qr1.leftJoin(qr2).on((a1, b1) => a1.s == b1.s)
          } yield {
            (a, b, c)
          }
        }
        AvoidAliasConflict(q.ast) mustEqual n.ast
      }
      "left" in {
        val q = quote {
          for {
            a <- qr1
            c <- qr1.leftJoin(qr2).on((a, b) => a.s == b.s)
          } yield {
            (a, c)
          }
        }
        val n = quote {
          for {
            a <- qr1
            c <- qr1.leftJoin(qr2).on((a1, b) => a1.s == b.s)
          } yield {
            (a, c)
          }
        }
        AvoidAliasConflict(q.ast) mustEqual n.ast
      }
      "right" in {
        val q = quote {
          for {
            b <- qr1
            c <- qr1.leftJoin(qr2).on((a, b) => a.s == b.s)
          } yield {
            (b, c)
          }
        }
        val n = quote {
          for {
            b <- qr1
            c <- qr1.leftJoin(qr2).on((a, b1) => a.s == b1.s)
          } yield {
            (b, c)
          }
        }
        AvoidAliasConflict(q.ast) mustEqual n.ast
      }
      "nested" in {
        val q = quote {
          qr1.map(t => t.i).leftJoin(qr2.map(t => t.i)).on((a, b) => a == b)
        }
        val n = quote {
          qr1.map(t => t.i).leftJoin(qr2.map(t1 => t1.i)).on((a, b) => a == b)
        }
        AvoidAliasConflict(q.ast) mustEqual n.ast
      }
      "multiple" in {
        val q = quote {
          qr1.leftJoin(qr2).on((a, b) => a.i == b.i)
            .leftJoin(qr1).on((a, b) => a._2.forall(v => v.i == b.i))
            .map(t => 1)
        }
        val n = quote {
          qr1.leftJoin(qr2).on((a, b) => a.i == b.i)
            .leftJoin(qr1).on((a1, b1) => a1._2.forall(v => v.i == b1.i))
            .map(t => 1)
        }
        AvoidAliasConflict(q.ast) mustEqual n.ast
      }
    }
  }

  "takes in consideration the aliases already defined" - {
    "flatMap" in {
      val q = quote {
        qr1.flatMap(a => qr2.flatMap(a => qr3))
      }
      val n = quote {
        qr1.flatMap(a => qr2.flatMap(a1 => qr3))
      }
      AvoidAliasConflict(q.ast) mustEqual n.ast
    }
    "map" in {
      val q = quote {
        qr1.map(a => a.s).flatMap(s => qr2.map(a => a.s))
      }
      val n = quote {
        qr1.map(a => a.s).flatMap(s => qr2.map(a1 => a1.s))
      }
      AvoidAliasConflict(q.ast) mustEqual n.ast
    }
    "filter" in {
      val q = quote {
        qr1.filter(a => a.s == "s").flatMap(s => qr2.map(a => a.s))
      }
      val n = quote {
        qr1.filter(a => a.s == "s").flatMap(s => qr2.map(a1 => a1.s))
      }
      AvoidAliasConflict(q.ast) mustEqual n.ast
    }
    "sortBy" in {
      val q = quote {
        qr1.sortBy(a => a.s).flatMap(s => qr2.map(a => a.s))
      }
      val n = quote {
        qr1.sortBy(a => a.s).flatMap(s => qr2.map(a1 => a1.s))
      }
      AvoidAliasConflict(q.ast) mustEqual n.ast
    }
    "outer join" - {
      "left" in {
        val q = quote {
          qr1.fullJoin(qr2.filter(a => a.i == 1)).on((b, c) => b.s == c.s).flatMap(d => qr2.map(b => b.s))
        }
        val n = quote {
          qr1.fullJoin(qr2.filter(a => a.i == 1)).on((b, c) => b.s == c.s).flatMap(d => qr2.map(b1 => b1.s))
        }
        AvoidAliasConflict(q.ast) mustEqual n.ast
      }
      "right" in {
        val q = quote {
          qr1.filter(a => a.i == 1).fullJoin(qr2).on((b, c) => b.s == c.s).flatMap(d => qr2.map(c => c.s))
        }
        val n = quote {
          qr1.filter(a => a.i == 1).fullJoin(qr2).on((b, c) => b.s == c.s).flatMap(d => qr2.map(c1 => c1.s))
        }
        AvoidAliasConflict(q.ast) mustEqual n.ast
      }
      "both" in {
        val q = quote {
          qr1.fullJoin(qr2).on((a, b) => a.s == b.s).flatMap(c => qr1.flatMap(a => qr2.map(b => b.s)))
        }
        val n = quote {
          qr1.fullJoin(qr2).on((a, b) => a.s == b.s).flatMap(c => qr1.flatMap(a1 => qr2.map(b1 => b1.s)))
        }
        AvoidAliasConflict(q.ast) mustEqual n.ast
      }
    }
  }

  "handles many alias conflicts" in {
    val q = quote {
      qr1.flatMap(a => qr2.flatMap(a => qr2.flatMap(a => qr1)))
    }
    val n = quote {
      qr1.flatMap(a => qr2.flatMap(a1 => qr2.flatMap(a2 => qr1)))
    }
    AvoidAliasConflict(q.ast) mustEqual n.ast
  }

  "doesn't change the query if it doesn't have conflicts" in {
    val q = quote {
      qr1.flatMap(a => qr2.sortBy(b => b.s).filter(c => c.s == "s1")).flatMap(d => qr3.map(e => e.s))
    }
    AvoidAliasConflict(q.ast) mustEqual q.ast
  }
}
