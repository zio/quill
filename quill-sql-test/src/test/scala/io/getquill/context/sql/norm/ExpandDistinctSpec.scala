package io.getquill.context.sql.norm

import io.getquill.{Ord, StatelessCache}
import io.getquill.base.Spec
import io.getquill.context.sql.testContext.qr1
import io.getquill.context.sql.testContext.quote
import io.getquill.context.sql.testContext.unquote
import io.getquill.util.TraceConfig

class ExpandDistinctSpec extends Spec {
  val ExpandDistinct = new ExpandDistinct(StatelessCache.NoCache, TraceConfig.Empty)

  "expands distinct map" - {
    "simple" in {
      val q = quote {
        qr1.map(e => e.i).distinct.nested
      }
      ExpandDistinct(q.ast).toString mustEqual
        """querySchema("TestEntity").map(e => (e.i)).distinct.map(e => e._1).nested"""
    }
    "aggregation" in {
      val q = quote {
        qr1.map(e => (e.i, e.l)).groupBy(g => g._1).map(_._2.max).distinct.nested
      }
      ExpandDistinct(q.ast).toString mustEqual
        """querySchema("TestEntity").map(e => (e.i, e.l)).groupBy(g => g._1).map(x1 => (x1._2.max)).distinct.map(x1 => x1._1).nested"""
    }
    "with case class" in {
      case class Rec(one: Int, two: Long)
      val q = quote {
        qr1.map(e => Rec(e.i, e.l)).distinct.nested
      }
      ExpandDistinct(q.ast).toString mustEqual
        """querySchema("TestEntity").map(e => Rec(one: e.i, two: e.l)).distinct.map(e => Rec(one: e.one, two: e.two)).nested"""
    }
    "with tuple" in {
      val q = quote {
        qr1.map(e => (e.i, e.l)).distinct.nested
      }
      ExpandDistinct(q.ast).toString mustEqual
        """querySchema("TestEntity").map(e => (e.i, e.l)).distinct.map(e => (e._1, e._2)).nested"""
    }
  }
}
