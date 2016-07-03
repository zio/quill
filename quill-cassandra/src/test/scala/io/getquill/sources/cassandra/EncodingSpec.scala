package io.getquill.sources.cassandra

import io.getquill._
import java.util.{ UUID, Date }
import monifu.reactive.Observable

class EncodingSpec extends Spec {

  "encodes and decodes types" - {

    "sync" in {
      import testSyncDB._
      testSyncDB.run(query[EncodingTestEntity].delete)
      testSyncDB.run(query[EncodingTestEntity].insert)(insertValues)
      verify(testSyncDB.run(query[EncodingTestEntity]))
    }

    "async" in {
      import testAsyncDB._
      import scala.concurrent.ExecutionContext.Implicits.global
      await {
        for {
          _ <- testAsyncDB.run(query[EncodingTestEntity].delete)
          _ <- testAsyncDB.run(query[EncodingTestEntity].insert)(insertValues)
          result <- testAsyncDB.run(query[EncodingTestEntity])
        } yield {
          verify(result)
        }
      }
    }

    "stream" in {
      import testStreamDB._
      import monifu.concurrent.Implicits.globalScheduler
      val result =
        for {
          _ <- testStreamDB.run(query[EncodingTestEntity].delete)
          inserts = Observable.from(insertValues: _*)
          _ <- testStreamDB.run(query[EncodingTestEntity].insert)(inserts).count
          result <- testStreamDB.run(query[EncodingTestEntity])
        } yield {
          result
        }
      val f = result.foldLeft(List.empty[EncodingTestEntity])(_ :+ _).asFuture
      verify(await(f).getOrElse(List()))
    }
  }

  "encodes collections" - {
    "sync" in {
      import testSyncDB._
      val q = quote {
        (list: List[Int]) =>
          query[EncodingTestEntity].filter(t => list.contains(t.id))
      }
      testSyncDB.run(query[EncodingTestEntity])
      testSyncDB.run(query[EncodingTestEntity].insert)(insertValues)
      verify(testSyncDB.run(q)(insertValues.map(_.id)))
    }

    "async" in {
      import testAsyncDB._
      import scala.concurrent.ExecutionContext.Implicits.global
      val q = quote {
        (list: List[Int]) =>
          query[EncodingTestEntity].filter(t => list.contains(t.id))
      }
      await {
        for {
          _ <- testAsyncDB.run(query[EncodingTestEntity].delete)
          _ <- testAsyncDB.run(query[EncodingTestEntity].insert)(insertValues)
          r <- testAsyncDB.run(q)(insertValues.map(_.id))
        } yield {
          verify(r)
        }
      }
    }

    "stream" in {
      import testStreamDB._
      import monifu.concurrent.Implicits.globalScheduler
      val q = quote {
        (list: List[Int]) =>
          query[EncodingTestEntity].filter(t => list.contains(t.id))
      }
      val result =
        for {
          _ <- testStreamDB.run(query[EncodingTestEntity].delete)
          inserts = Observable.from(insertValues: _*)
          _ <- testStreamDB.run(query[EncodingTestEntity].insert)(inserts).count
          result <- testStreamDB.run(q)(insertValues.map(_.id))
        } yield {
          result
        }
      val f = result.foldLeft(List.empty[EncodingTestEntity])(_ :+ _).asFuture
      verify(await(f).getOrElse(List()))
    }
  }

  private def verify(result: List[EncodingTestEntity]): Unit =
    result.zip(insertValues) match {
      case List((e1, a1), (e2, a2)) =>
        verify(e1, a1)
        verify(e2, a2)
    }

  private def verify(e: EncodingTestEntity, a: EncodingTestEntity): Unit = {
    e.id mustEqual a.id

    e.v1 mustEqual a.v1
    e.v2 mustEqual a.v2
    e.v3 mustEqual a.v3
    e.v4 mustEqual a.v4
    e.v5 mustEqual a.v5
    e.v6 mustEqual a.v6
    e.v7 mustEqual a.v7
    e.v8.toList mustEqual a.v8.toList
    e.v9 mustEqual a.v9
    e.v10 mustEqual a.v10
    e.o1 mustEqual a.o1
    e.o2 mustEqual a.o2
    e.o3 mustEqual a.o3
    e.o4 mustEqual a.o4
    e.o5 mustEqual a.o5
    e.o6 mustEqual a.o6
    e.o7 mustEqual a.o7
    e.o8.map(_.toList) mustEqual a.o8.map(_.toList)
    e.o9 mustEqual a.o9
  }

  case class EncodingTestEntity(
    id:  Int,
    v1:  String,
    v2:  BigDecimal,
    v3:  Boolean,
    v4:  Int,
    v5:  Long,
    v6:  Float,
    v7:  Double,
    v8:  Array[Byte],
    v9:  Date,
    v10: UUID,
    o1:  Option[String],
    o2:  Option[BigDecimal],
    o3:  Option[Boolean],
    o4:  Option[Int],
    o5:  Option[Long],
    o6:  Option[Float],
    o7:  Option[Double],
    o8:  Option[Array[Byte]],
    o9:  Option[Date]
  )

  private val fixUUID: UUID = UUID.fromString("606c79e8-a331-4810-8bd7-0668ff7a23ef")

  val insertValues =
    List(
      EncodingTestEntity(
        id = 1,
        v1 = "s",
        v2 = BigDecimal(1.1),
        v3 = true,
        v4 = 33,
        v5 = 431L,
        v6 = 34.4f,
        v7 = 42d,
        v8 = Array(1.toByte, 2.toByte),
        v9 = new Date(31200000),
        v10 = fixUUID,
        o1 = Some("s"),
        o2 = Some(BigDecimal(1.1)),
        o3 = Some(true),
        o4 = Some(33),
        o5 = Some(431L),
        o6 = Some(34.4f),
        o7 = Some(42d),
        o8 = Some(Array(1.toByte, 2.toByte)),
        o9 = Some(new Date(31200000))
      ),
      EncodingTestEntity(
        id = 2,
        v1 = "",
        v2 = BigDecimal(0),
        v3 = false,
        v4 = 0,
        v5 = 0L,
        v6 = 0F,
        v7 = 0D,
        v8 = Array(),
        v9 = new Date(0),
        v10 = fixUUID,
        o1 = None,
        o2 = None,
        o3 = None,
        o4 = None,
        o5 = None,
        o6 = None,
        o7 = None,
        o8 = None,
        o9 = None
      )
    )
}
