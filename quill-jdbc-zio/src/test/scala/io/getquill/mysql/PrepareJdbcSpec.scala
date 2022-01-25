package io.getquill.mysql

import java.sql.{ Connection, ResultSet }
import io.getquill.PrepareZioJdbcSpecBase
import io.getquill.Prefix
import org.scalatest.BeforeAndAfter

class PrepareJdbcSpec extends PrepareZioJdbcSpecBase with BeforeAndAfter {

  def prefix = Prefix("testMysqlDB")
  val context = testContext
  import context._

  before {
    testContext.run(query[Product].delete).runSyncUnsafe()
  }

  def productExtractor = (rs: ResultSet, conn: Connection) => materializeQueryMeta[Product].extract(rs, conn)
  val prepareQuery = prepare(query[Product])

  "single" in {
    val prepareInsert = prepare(query[Product].insertValue(lift(productEntries.head)))
    singleInsert(prepareInsert) mustEqual false
    extractProducts(prepareQuery) === List(productEntries.head)
  }

  "batch" in {
    val prepareBatchInsert = prepare(
      liftQuery(withOrderedIds(productEntries)).foreach(p => query[Product].insertValue(p))
    )

    batchInsert(prepareBatchInsert).distinct mustEqual List(false)
    extractProducts(prepareQuery) === withOrderedIds(productEntries)
  }
}
