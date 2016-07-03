package io.getquill.context.jdbc.h2

import io.getquill.context.sql.EncodingSpec

class JdbcEncodingSpec extends EncodingSpec {

  val context = testContext
  import testContext._

  "encodes and decodes types" in {
    testContext.run(delete)
    testContext.run(insert)(insertValues)
    verify(testContext.run(query[EncodingTestEntity]))
  }
}
