package io.getquill.context.finagle.mysql

import io.getquill.context.sql._
import com.twitter.util.{ Await, Future }
import java.util.concurrent.ConcurrentLinkedQueue
import scala.collection.JavaConverters._

class QueryResultTypeFinagleMysqlSpec extends QueryResultTypeSpec {

  val context = testContext
  import testContext._

  def await[T](r: Future[T]) = Await.result(r)

  val insertedProducts = new ConcurrentLinkedQueue[Product]

  override def beforeAll = {
    await(testContext.run(deleteAll))
    val rs = await(testContext.run(productInsert)(productEntries))
    val inserted = (rs zip productEntries).map {
      case (r, prod) => prod.copy(id = r)
    }
    insertedProducts.addAll(inserted.asJava)
    ()
  }

  def products = insertedProducts.asScala.toList

  "return list" - {
    "select" in {
      await(testContext.run(selectAll)) must contain theSameElementsAs (products)
    }
    "map" in {
      await(testContext.run(map)) must contain theSameElementsAs (products.map(_.id))
    }
    "filter" in {
      await(testContext.run(filter)) must contain theSameElementsAs (products)
    }
    "withFilter" in {
      await(testContext.run(withFilter)) must contain theSameElementsAs (products)
    }
    "sortBy" in {
      await(testContext.run(sortBy)) must contain theSameElementsInOrderAs (products)
    }
    "take" in {
      await(testContext.run(take)) must contain theSameElementsAs (products)
    }
    "drop" in {
      await(testContext.run(drop)) must contain theSameElementsAs (products.drop(1))
    }
    "++" in {
      await(testContext.run(`++`)) must contain theSameElementsAs (products ++ products)
    }
    "unionAll" in {
      await(testContext.run(unionAll)) must contain theSameElementsAs (products ++ products)
    }
    "union" in {
      await(testContext.run(union)) must contain theSameElementsAs (products)
    }
    "join" in {
      await(testContext.run(join)) must contain theSameElementsAs (products zip products)
    }
    "distinct" in {
      await(testContext.run(distinct)) must contain theSameElementsAs (products.map(_.id).distinct)
    }
  }

  "return single result" - {
    "min" - {
      "some" in {
        await(testContext.run(minExists)) mustEqual Some(products.map(_.sku).min)
      }
      "none" in {
        await(testContext.run(minNonExists)) mustBe None
      }
    }
    "max" - {
      "some" in {
        await(testContext.run(maxExists)) mustBe Some(products.map(_.sku).max)
      }
      "none" in {
        await(testContext.run(maxNonExists)) mustBe None
      }
    }
    "avg" - {
      "some" in {
        await(testContext.run(avgExists)) mustBe Some(BigDecimal(products.map(_.sku).sum) / products.size)
      }
      "none" in {
        await(testContext.run(avgNonExists)) mustBe None
      }
    }
    "size" in {
      await(testContext.run(productSize)) mustEqual products.size
    }
    "parametrized size" in {
      await(testContext.run(parametrizedSize)(10000)) mustEqual 0
    }
    "nonEmpty" in {
      await(testContext.run(nonEmpty)) mustEqual true
    }
    "isEmpty" in {
      await(testContext.run(isEmpty)) mustEqual false
    }
  }
}
