package io.getquill.sources.cassandra

import monifu.reactive.Observable
import org.scalatest.BeforeAndAfterAll
import org.scalatest.FreeSpec
import org.scalatest.MustMatchers
import scala.concurrent.{ Await, Future }
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global
import io.getquill.naming.NamingStrategy

class QueryResultTypeCassandraSpec extends FreeSpec with BeforeAndAfterAll with MustMatchers {

  case class OrderTestEntity(id: Int, i: Int)

  val entries = List(
    OrderTestEntity(1, 1),
    OrderTestEntity(2, 2),
    OrderTestEntity(3, 3)
  )

  case class Context[N <: NamingStrategy, R, S](source: CassandraSource[N, R, S]) {

    import source._

    val insert = quote(query[OrderTestEntity].insert)
    val deleteAll = quote(query[OrderTestEntity].delete)
    val selectAll = quote(query[OrderTestEntity])
    val map = quote(query[OrderTestEntity].map(_.id))
    val filter = quote(query[OrderTestEntity].filter(_.id == 1))
    val withFilter = quote(query[OrderTestEntity].withFilter(_.id == 1))
    val sortBy = quote(query[OrderTestEntity].filter(_.id == 1).sortBy(_.i)(Ord.asc))
    val take = quote(query[OrderTestEntity].take(10))
    val entitySize = quote(query[OrderTestEntity].size)
    val parametrizedSize = quote { (id: Int) =>
      query[OrderTestEntity].filter(_.id == id).size
    }
    val distinct = quote(query[OrderTestEntity].map(_.id).distinct)
  }

  override def beforeAll = {
    val ctx = Context(testSyncDB)
    import ctx._
    val r1 = testSyncDB.run(deleteAll)
    val r2 = testSyncDB.run(insert)(entries)
  }

  "async" - {
    val db = testAsyncDB
    val ctx = Context(db)
    import ctx._
    def await[T](r: Future[T]) = Await.result(r, Duration.Inf)
    "return list" - {
      "select" in {
        await(db.run(selectAll)) must contain theSameElementsAs (entries)
      }
      "map" in {
        await(db.run(map)) must contain theSameElementsAs (entries.map(_.id))
      }
      "filter" in {
        await(db.run(filter)) mustEqual entries.take(1)
      }
      "withFilter" in {
        await(db.run(withFilter)) mustEqual entries.take(1)
      }
      "sortBy" in {
        await(db.run(sortBy)) mustEqual entries.take(1)
      }
      "take" in {
        await(db.run(take)) must contain theSameElementsAs (entries)
      }
    }

    "return single result" - {
      "size" in {
        await(db.run(entitySize)) mustEqual entries.size
      }
      "paramlize size" in {
        await(db.run(parametrizedSize)(10000)) mustEqual 0
      }
    }
  }

  "sync" - {
    val db = testSyncDB
    val ctx = Context(db)
    import ctx._
    def await[T](r: T) = r
    "return list" - {
      "select" in {
        await(db.run(selectAll)) must contain theSameElementsAs (entries)
      }
      "map" in {
        await(db.run(map)) must contain theSameElementsAs (entries.map(_.id))
      }
      "filter" in {
        await(db.run(filter)) mustEqual entries.take(1)
      }
      "withFilter" in {
        await(db.run(withFilter)) mustEqual entries.take(1)
      }
      "sortBy" in {
        await(db.run(sortBy)) mustEqual entries.take(1)
      }
      "take" in {
        await(db.run(take)) must contain theSameElementsAs (entries)
      }
    }

    "return single result" - {
      "size" in {
        await(db.run(entitySize)) mustEqual entries.size
      }
      "paramlize size" in {
        await(db.run(parametrizedSize)(10000)) mustEqual 0
      }
    }
  }
  "stream" - {
    import monifu.concurrent.Implicits.globalScheduler
    val db = testStreamDB
    val ctx = Context(db)
    import ctx._
    def await[T](t: Observable[T]) = {
      val f = t.foldLeft(List.empty[T])(_ :+ _).asFuture
      Await.result(f, Duration.Inf)
    }
    "query" in {
      await(db.run(selectAll)) mustEqual Some(entries)
    }

    "querySingle" - {
      "size" in {
        await(db.run(entitySize)) mustEqual Some(List(3))
      }
      "parametrized size" in {
        await(db.run(parametrizedSize)(10000)) mustEqual Some(List(0))
      }
    }
  }
}
