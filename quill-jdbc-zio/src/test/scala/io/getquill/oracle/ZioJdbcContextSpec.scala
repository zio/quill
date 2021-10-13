package io.getquill.oracle

import io.getquill.ZioSpec
import io.getquill.Prefix
import zio.{ Task, ZIO }

class ZioJdbcContextSpec extends ZioSpec {

  def prefix = Prefix("testOracleDB")
  val context = testContext
  import testContext._

  "provides transaction support" - {
    "success" in {
      (for {
        _ <- testContext.run(qr1.delete)
        _ <- testContext.transaction {
          testContext.run(qr1.insert(_.i -> 33))
        }
        r <- testContext.run(qr1)
      } yield r).runSyncUnsafe().map(_.i) mustEqual List(33)
    }
    "success - stream" in {
      (for {
        _ <- testContext.run(qr1.delete)
        seq <- testContext.transaction {
          for {
            _ <- testContext.run(qr1.insert(_.i -> 33))
            s <- accumulate(testContext.stream(qr1))
          } yield s
        }
        r <- testContext.run(qr1)
      } yield (seq.map(_.i), r.map(_.i))).runSyncUnsafe() mustEqual ((List(33), List(33)))
    }
    "failure" in {
      (for {
        _ <- testContext.run(qr1.delete)
        e <- testContext.transaction {
          ZIO.collectAll(Seq(
            testContext.run(qr1.insert(_.i -> 18)),
            Task {
              throw new IllegalStateException
            }
          ))
        }.catchSome {
          case e: Exception => Task(e.getClass.getSimpleName)
        }
        r <- testContext.run(qr1)
      } yield (e, r.isEmpty)).runSyncUnsafe() mustEqual (("IllegalStateException", true))
    }
    "nested" in {
      (for {
        _ <- testContext.run(qr1.delete)
        _ <- testContext.transaction {
          testContext.transaction {
            testContext.run(qr1.insert(_.i -> 33))
          }
        }
        r <- testContext.run(qr1)
      } yield r).runSyncUnsafe().map(_.i) mustEqual List(33)
    }
    "prepare" in {
      testContext.prepareParams(
        "select * from Person where name=? and age > ?", ps => (List("Sarah", 127), ps)
      ).runSyncUnsafe() mustEqual List("127", "'Sarah'")
    }
  }
}
