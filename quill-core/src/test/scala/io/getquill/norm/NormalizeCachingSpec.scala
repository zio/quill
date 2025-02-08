package io.getquill.norm

import io.getquill.base.Spec

class NormalizeCachingSpec extends Spec {

  val normalize = new Normalize(NormalizeCaches.noCache, TranspileConfig.Empty)
  val cached    = NormalizeCaching(normalize.apply)
  val gen       = new QueryGenerator(1)

  "Cached normalization" - {
    "consists with non-cached `Normalize`" in {
      for (i <- (3 to 15)) {
        for (j <- (0 until 30)) {
          val query = gen(i)
          val r     = normalize(query)
          val cr    = cached.apply(query)
          r mustEqual (cr)
        }
      }
    }
  }
}
