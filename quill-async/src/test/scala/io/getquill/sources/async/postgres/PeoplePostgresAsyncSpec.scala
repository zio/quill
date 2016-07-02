package io.getquill.sources.async.postgres

import io.getquill._
import io.getquill.sources.sql.PeopleSpec
import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.{ global => ec }

class PeoplePostgresAsyncSpec extends PeopleSpec {

  def await[T](future: Future[T]) = Await.result(future, Duration.Inf)

  override def beforeAll =
    await {
      testPostgresDB.transaction { implicit ec =>
        for {
          _ <- testPostgresDB.run(query[Couple].delete)
          _ <- testPostgresDB.run(query[Person].filter(_.age > 0).delete)
          _ <- testPostgresDB.run(peopleInsert)(peopleEntries)
          _ <- testPostgresDB.run(couplesInsert)(couplesEntries)
        } yield {}
      }
    }

  "Example 1 - differences" in {
    await(testPostgresDB.run(`Ex 1 differences`)) mustEqual `Ex 1 expected result`
  }

  "Example 2 - range simple" in {
    await(testPostgresDB.run(`Ex 2 rangeSimple`)(`Ex 2 param 1`, `Ex 2 param 2`)) mustEqual `Ex 2 expected result`
  }

  "Examples 3 - satisfies" in {
    await(testPostgresDB.run(`Ex 3 satisfies`)) mustEqual `Ex 3 expected result`
  }

  "Examples 4 - satisfies" in {
    await(testPostgresDB.run(`Ex 4 satisfies`)) mustEqual `Ex 4 expected result`
  }

  "Example 5 - compose" in {
    await(testPostgresDB.run(`Ex 5 compose`)(`Ex 5 param 1`, `Ex 5 param 2`)) mustEqual `Ex 5 expected result`
  }

  "Example 6 - predicate 0" in {
    await(testPostgresDB.run(satisfies(eval(`Ex 6 predicate`)))) mustEqual `Ex 6 expected result`
  }

  "Example 7 - predicate 1" in {
    await(testPostgresDB.run(satisfies(eval(`Ex 7 predicate`)))) mustEqual `Ex 7 expected result`
  }

  "Example 8 - contains empty" in {
    await(testPostgresDB.run(`Ex 8 and 9 contains`)(`Ex 8 param`)) mustEqual `Ex 8 expected result`
  }

  "Example 9 - contains non empty" in {
    await(testPostgresDB.run(`Ex 8 and 9 contains`)(`Ex 9 param`)) mustEqual `Ex 9 expected result`
  }
}
