package io.getquill.context.finagle.mysql

import java.util.{ Date, TimeZone }

import com.twitter.util.Await
import io.getquill.context.sql.EncodingSpec
import io.getquill.util.LoadConfig
import io.getquill.{ FinagleMysqlContext, FinagleMysqlContextConfig, Literal }

import scala.concurrent.duration._

class FinagleMysqlEncodingSpec extends EncodingSpec {

  val context = testContext
  import testContext._

  "encodes and decodes types" in {
    val r =
      for {
        _ <- testContext.run(delete)
        _ <- testContext.run(liftQuery(insertValues).foreach(e => insert(e)))
        result <- testContext.run(query[EncodingTestEntity])
      } yield result

    verify(Await.result(r).toList)
  }

  "fails if the column has the wrong type" in {
    Await.result(testContext.run(liftQuery(insertValues).foreach(e => insert(e))))
    case class EncodingTestEntity(v1: Int)
    val e = intercept[IllegalStateException] {
      Await.result(testContext.run(query[EncodingTestEntity]))
    }
  }

  "encodes sets" in {
    val q = quote {
      (set: Query[Int]) =>
        query[EncodingTestEntity].filter(t => set.contains(t.v6))
    }
    Await.result {
      for {
        _ <- testContext.run(query[EncodingTestEntity].delete)
        _ <- testContext.run(liftQuery(insertValues).foreach(e => query[EncodingTestEntity].insert(e)))
        r <- testContext.run(q(liftQuery(insertValues.map(_.v6))))
      } yield {
        verify(r)
      }
    }
  }

  "Integer type with Long" in {
    case class IntLong(o6: Long)
    val entity = quote { querySchema[IntLong]("EncodingTestEntity") }
    val insert = quote {
      entity.insert(_.o6 -> 5589)
    }
    val q = quote {
      entity.filter(_.o6 == 5589)
    }
    Await.result(testContext.run(insert))
    Await.result(testContext.run(q)).nonEmpty mustEqual true
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
      val r = for {
        _ <- testContext.run(query[BooleanEncodingTestEntity].delete)
        _ <- testContext.run(query[BooleanEncodingTestEntity].insert(lift(entity)))
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

  "decode data types" - {
    case class DateEncodingTestEntity(
      v1: Date,
      v2: Date,
      v3: Date
    )

    val date = new Date
    val entity = DateEncodingTestEntity(date, date, date)

    def round(milliseconds: Long, duration: Duration): Long = Math.round(milliseconds / duration.toMillis.toDouble) * duration.toMillis

    def verify(result: DateEncodingTestEntity) = {
      round(result.v1.getTime, 24.hours) mustEqual round(entity.v1.getTime, 24.hours)
      result.v2.getTime mustEqual round(entity.v2.getTime, 1.second)
      result.v3.getTime mustEqual round(entity.v3.getTime, 1.second)
    }

    "default timezone" in {
      val r = for {
        _ <- testContext.run(query[DateEncodingTestEntity].delete)
        _ <- testContext.run(query[DateEncodingTestEntity].insert(lift(entity)))
        result <- testContext.run(query[DateEncodingTestEntity])
      } yield result

      verify(Await.result(r).head)
    }

    "different timezone" in {
      val config = FinagleMysqlContextConfig(LoadConfig("testDB"))
      val testTimezoneContext = new FinagleMysqlContext[Literal](config.client, TimeZone.getTimeZone("KST"), TimeZone.getTimeZone("UTC"))
      import testTimezoneContext._

      val r = for {
        _ <- testTimezoneContext.run(query[DateEncodingTestEntity].delete)
        _ <- testTimezoneContext.run(query[DateEncodingTestEntity].insert(lift(entity)))
        result <- testTimezoneContext.run(query[DateEncodingTestEntity])
      } yield result

      verify(Await.result(r).head)
    }
  }
}
