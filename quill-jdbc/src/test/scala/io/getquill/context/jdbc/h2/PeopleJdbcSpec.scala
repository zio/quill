package io.getquill.context.jdbc.h2

import io.getquill.context.sql.PeopleSpec

class PeopleJdbcSpec extends PeopleSpec {

  val context = testContext
  import testContext._

  override def beforeAll = {
    testContext.transaction {
      testContext.run(query[Couple].delete)
      testContext.run(query[Person].filter(_.age > 0).delete)
      testContext.run(peopleInsert)(peopleEntries)
      testContext.run(couplesInsert)(couplesEntries)
    }
    ()
  }

  "Example 1 - differences" in {
    testContext.run(`Ex 1 differences`) mustEqual `Ex 1 expected result`
  }

  "Example 2 - range simple" in {
    testContext.run(`Ex 2 rangeSimple`)(`Ex 2 param 1`, `Ex 2 param 2`) mustEqual `Ex 2 expected result`
  }

  "Example 3 - satisfies" in {
    testContext.run(`Ex 3 satisfies`) mustEqual `Ex 3 expected result`
  }

  "Example 4 - satisfies" in {
    testContext.run(`Ex 4 satisfies`) mustEqual `Ex 4 expected result`
  }

  "Example 5 - compose" in {
    testContext.run(`Ex 5 compose`)(`Ex 5 param 1`, `Ex 5 param 2`) mustEqual `Ex 5 expected result`
  }

  "Example 6 - predicate 0" in {
    testContext.run(satisfies(eval(`Ex 6 predicate`))) mustEqual `Ex 6 expected result`
  }

  "Example 7 - predicate 1" in {
    testContext.run(satisfies(eval(`Ex 7 predicate`))) mustEqual `Ex 7 expected result`
  }

  "Example 8 - contains empty" in {
    testContext.run(`Ex 8 and 9 contains`)(`Ex 8 param`) mustEqual `Ex 8 expected result`
  }

  "Example 9 - contains non empty" in {
    testContext.run(`Ex 8 and 9 contains`)(`Ex 9 param`) mustEqual `Ex 9 expected result`
  }
}
