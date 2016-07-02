package io.getquill.context.finagle.mysql

import com.twitter.util.Await

import io.getquill.context.sql.EncodingSpec

class FinagleMysqlEncodingSpec extends EncodingSpec {

  val context = testContext
  import testContext._

  "encodes and decodes types" in {
    val r =
      for {
        _ <- testContext.run(delete)
        _ <- testContext.run(insert)(insertValues)
        result <- testContext.run(query[EncodingTestEntity])
      } yield result

    verify(Await.result(r).toList)
  }

  "fails if the column has the wrong type" in {
    Await.result(testContext.run(insert)(insertValues))
    case class EncodingTestEntity(v1: Int)
    val e = intercept[IllegalStateException] {
      Await.result(testContext.run(query[EncodingTestEntity]))
    }
  }

  "encodes sets" in {
    val q = quote {
      (set: Set[Int]) =>
        query[EncodingTestEntity].filter(t => set.contains(t.v6))
    }
    Await.result {
      for {
        _ <- testContext.run(query[EncodingTestEntity].delete)
        _ <- testContext.run(query[EncodingTestEntity].insert)(insertValues)
        r <- testContext.run(q)(insertValues.map(_.v6).toSet)
      } yield {
        verify(r)
      }
    }
  }

  "decode boolean types" - {
    case class BooleanEncodingTestEntity(
      v1: Boolean,
      v2: Boolean,
      v3: Boolean,
      v4: Boolean,
      v5: Boolean,
      v6: Boolean,
      v7: Boolean
    )
    val decodeBoolean = (entity: BooleanEncodingTestEntity) => {
      val delete = quote(query[BooleanEncodingTestEntity].delete)
      val insert = quote(query[BooleanEncodingTestEntity].insert)
      val r = for {
        _ <- testContext.run(delete)
        _ <- testContext.run(insert)(List(entity))
        result <- testContext.run(query[BooleanEncodingTestEntity])
      } yield result
      Await.result(r).head
    }
    "true" in {
      val entity = BooleanEncodingTestEntity(true, true, true, true, true, true, true)
      val r = decodeBoolean(entity)
      r.v1 mustEqual true
      r.v2 mustEqual true
      r.v3 mustEqual true
      r.v4 mustEqual true
      r.v5 mustEqual true
      r.v6 mustEqual true
      r.v7 mustEqual true
    }

    "false" in {
      val entity = BooleanEncodingTestEntity(false, false, false, false, false, false, false)
      val r = decodeBoolean(entity)
      r.v1 mustEqual false
      r.v2 mustEqual false
      r.v3 mustEqual false
      r.v4 mustEqual false
      r.v5 mustEqual false
      r.v6 mustEqual false
      r.v7 mustEqual false
    }
  }
}
