package io.getquill.context

import io.getquill.Spec
import io.getquill.context.mirror.Row
import io.getquill.testContext
import io.getquill.testContext._

case class WrappedEncodable(value: Int)
  extends AnyVal

class ContextInstanceSpec extends Spec {

  "provides mapped encoding" - {

    case class StringValue(s: String)
    case class Entity(s: StringValue)

    "context-based" - {
      "encoding" in {
        implicit val testToString = MappedEncoding[StringValue, String](_.s)
        val q = quote {
          query[Entity].insert(_.s -> lift(StringValue("s")))
        }
        testContext.run(q).prepareRow mustEqual Row("s")
      }

      "decoding" in {
        implicit val stringToTest = MappedEncoding[String, StringValue](StringValue)
        val q = quote {
          query[Entity]
        }
        testContext.run(q).extractor(Row("s")) mustEqual Entity(StringValue("s"))
      }
    }
    "package-based" - {
      import io.getquill.MappedEncoding
      "encoding" in {
        implicit val testToString = MappedEncoding[StringValue, String](_.s)
        val q = quote {
          query[Entity].insert(_.s -> lift(StringValue("s")))
        }
        testContext.run(q).prepareRow mustEqual Row("s")
      }

      "decoding" in {
        implicit val stringToTest = MappedEncoding[String, StringValue](StringValue)
        val q = quote {
          query[Entity]
        }
        testContext.run(q).extractor(Row("s")) mustEqual Entity(StringValue("s"))
      }
    }
  }

  "encoding set" in {
    case class Entity(i: Int)
    val q = quote {
      query[Entity].filter(e => liftQuery(Set(1)).contains(e.i))
    }
    testContext.run(q).prepareRow mustEqual Row(1)
  }

  "encodes `WrappedValue` extended value class" - {
    case class Entity(x: WrappedEncodable, s: String)

    "encoding" in {
      val q = quote {
        query[Entity].insert(_.x -> lift(WrappedEncodable(1)), _.s -> s"string")
      }
      testContext.run(q).prepareRow mustEqual Row(1)
    }

    "decoding" in {
      val q = quote {
        query[Entity]
      }
      val wrapped = WrappedEncodable(1)
      testContext.run(q).extractor(Row(1, "1")) mustEqual Entity(wrapped, "1")
    }
  }
}
