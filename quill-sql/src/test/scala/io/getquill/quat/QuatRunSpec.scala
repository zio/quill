package io.getquill.quat

import io.getquill._

class QuatRunSpec extends Spec {

  val testContext = new SqlMirrorContext(PostgresDialect, Literal)
  import testContext._

  "should refine quats from generic infixes and express during execution" - {
    case class MyPerson(name: String, age: Int)
    val MyPersonQuat = Quat.Product("name" -> Quat.Value, "age" -> Quat.Value)

    "from extension methods" in {
      implicit class QueryOps[Q <: Query[_]](q: Q) {
        def appendFoo = quote(infix"$q APPEND FOO".pure.as[Q])
      }
      val q = quote(query[MyPerson].appendFoo)
      q.ast.quat mustEqual MyPersonQuat // I.e ReifyLiftings runs RepropagateQuats to take care of this
      testContext.run(q).string mustEqual "SELECT x.name, x.age FROM MyPerson x APPEND FOO"
    }

    "from extension methods - generic marker" in {
      implicit class QueryOps[Q <: Query[_]](q: Q) {
        def appendFoo = quote(infix"$q APPEND FOO".generic.pure.as[Q])
      }
      val q = quote(query[MyPerson].appendFoo)
      q.ast.quat mustEqual MyPersonQuat // I.e ReifyLiftings runs RepropagateQuats to take care of this
      testContext.run(q).string mustEqual "SELECT x.name, x.age FROM MyPerson x APPEND FOO"
    }

    "should support query-ops function" in {
      def appendFooFun[Q <: Query[_]]: Quoted[Q => Q] = quote { (q: Q) => infix"$q APPEND FOO".pure.as[Q] }
      val q = quote(appendFooFun(query[MyPerson]))
      q.ast.quat mustEqual Quat.Unknown
      testContext.run(q).string mustEqual "SELECT x.name, x.age FROM MyPerson x APPEND FOO"
    }

    "should support query-ops function - dynamic function" in {
      def appendFooFun[Q <: Query[_]] = quote { (q: Q) => infix"$q APPEND FOO".pure.as[Q] }
      val q = quote(appendFooFun(query[MyPerson]))
      q.ast.quat mustEqual MyPersonQuat
      testContext.run(q).string mustEqual "SELECT x.name, x.age FROM MyPerson x APPEND FOO"
    }
  }
}
