package io.getquill.context.finagle.postgres

import com.twitter.util.{ Await, Future }
import io.getquill.context.sql.base.OnConflictSpec

class OnConflictFinagleSpec extends OnConflictSpec {
  val ctx = testContext
  import ctx._

  def await[T](future: Future[T]) = Await.result(future)

  override protected def beforeAll(): Unit = {
    await(ctx.run(qr1.delete))
    ()
  }

  "ON CONFLICT DO NOTHING" in {
    import `onConflictIgnore`._
    await(ctx.run(testQuery1)) mustEqual res1
    await(ctx.run(testQuery2)) mustEqual res2
    await(ctx.run(testQuery3)) mustEqual res3
  }

  "ON CONFLICT (i) DO NOTHING" in {
    import `onConflictIgnore(_.i)`._
    await(ctx.run(testQuery1)) mustEqual res1
    await(ctx.run(testQuery2)) mustEqual res2
    await(ctx.run(testQuery3)) mustEqual res3
  }

  "ON CONFLICT (i) DO UPDATE ..." in {
    import `onConflictUpdate(_.i)((t, e) => ...)`._
    await(ctx.run(testQuery(e1))) mustEqual res1
    await(ctx.run(testQuery(e2))) mustEqual res2
    await(ctx.run(testQuery(e3))) mustEqual res3
    await(ctx.run(testQuery4)) mustEqual res4
  }
}
