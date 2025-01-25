package io.getquill.norm.capture

import io.getquill.base.Spec
import io.getquill.MirrorContexts.testContext.qr1
import io.getquill.MirrorContexts.testContext.qr2
import io.getquill.MirrorContexts.testContext.qr3
import io.getquill.MirrorContexts.testContext.quote
import io.getquill.MirrorContexts.testContext.unquote
import io.getquill.StatefulCache
import io.getquill.util.TraceConfig

class AvoidCaptureSpec extends Spec {

  "avoids capture of entities for normalization" in {
    val q = quote {
      qr1.filter(u => u.s == "s1").flatMap(b => qr2.filter(u => u.s == "s1")).flatMap(c => qr3.map(u => u.s))
    }
    val n = quote {
      qr1.filter(u => u.s == "s1").flatMap(u => qr2.filter(u1 => u1.s == "s1")).flatMap(u1 => qr3.map(u2 => u2.s))
    }
    AvoidCapture(q.ast, StatefulCache.NoCache, TraceConfig(List.empty)) mustEqual n.ast
  }
}
