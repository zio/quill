package io.getquill.norm

import io.getquill.ast.{+&&+, _}
import io.getquill.MirrorContexts.testContext._
import io.getquill.ast.Implicits._
import io.getquill.norm.ConcatBehavior.{AnsiConcat, NonAnsiConcat}
import io.getquill.MoreAstOps._
import io.getquill.StatelessCache
import io.getquill.base.Spec
import io.getquill.util.TraceConfig

class FlattenOptionOperationSpec extends Spec {

  def o      = Ident("o")
  def c1     = Constant.auto(1)
  def c2     = Constant.auto(2)
  def cFoo   = Constant.auto("foo")
  def cBar   = Constant.auto("bar")
  def cValue = Constant.auto("value")

  "transforms option operations into simple properties" - {
    case class Row(id: Int, value: String)

    "getOrElse" in {
      val q = quote { (o: Option[Int]) =>
        o.getOrElse(1)
      }
      new FlattenOptionOperation(StatelessCache.NoCache, AnsiConcat, TraceConfig.Empty)(q.ast.body: Ast) mustEqual
        IfExist(o, o, c1)
    }
    "orElse" - {
      "regular operation" in {
        val q = quote { (o: Option[Int]) =>
          o.orElse(Option(1))
        }
        new FlattenOptionOperation(StatelessCache.NoCache, AnsiConcat, TraceConfig.Empty)(q.ast.body: Ast) mustEqual
          IfExist(o, o, c1)
      }
      "with forall" in {
        val q = quote { (o: Option[Int]) =>
          o.orElse(Option(1)).forall(_ == 2)
        }
        new FlattenOptionOperation(StatelessCache.NoCache, AnsiConcat, TraceConfig.Empty)(q.ast.body: Ast) mustEqual
          ((o +==+ c2) +||+ (IsNullCheck(o) +&&+ (c1 +==+ c2))
            +||+ (IsNullCheck(o) +&&+ IsNullCheck(c1)))
      }
      "with exists" in {
        val q = quote { (o: Option[Int]) =>
          o.orElse(Option(1)).exists(_ == 2)
        }
        new FlattenOptionOperation(StatelessCache.NoCache, AnsiConcat, TraceConfig.Empty)(q.ast.body: Ast) mustEqual
          ((o +==+ c2 +&&+ IsNotNullCheck(o)) +||+ (c1 +==+ c2 +&&+ IsNotNullCheck(c1)))
      }
    }
    "flatten" - {
      "regular operation" in {
        val q = quote { (o: Option[Option[Int]]) =>
          o.flatten.map(i => i + 1)
        }
        new FlattenOptionOperation(StatelessCache.NoCache, AnsiConcat, TraceConfig.Empty)(q.ast.body: Ast) mustEqual
          o +++ c1
        new FlattenOptionOperation(StatelessCache.NoCache, NonAnsiConcat, TraceConfig.Empty)(q.ast.body: Ast) mustEqual
          o +++ c1
      }
      "possible-fallthrough operation" in {
        val q = quote { (o: Option[Option[String]]) =>
          o.flatten.map(s => s + "foo")
        }
        new FlattenOptionOperation(StatelessCache.NoCache, AnsiConcat, TraceConfig.Empty)(q.ast.body: Ast) mustEqual
          o +++ Constant.auto("foo")
        new FlattenOptionOperation(StatelessCache.NoCache, NonAnsiConcat, TraceConfig.Empty)(q.ast.body: Ast) mustEqual
          IfExistElseNull(o, o +++ cFoo)
      }
      "never-fallthrough operation" in {
        val q = quote { (o: Option[Option[String]]) =>
          o.flatten.map(s => if (s == "value") "foo" else "bar")
        }
        new FlattenOptionOperation(StatelessCache.NoCache, AnsiConcat, TraceConfig.Empty)(q.ast.body: Ast) mustEqual
          IfExist(o, If(o +==+ cValue, cFoo, cBar), NullValue)
        new FlattenOptionOperation(StatelessCache.NoCache, NonAnsiConcat, TraceConfig.Empty)(q.ast.body: Ast) mustEqual
          IfExist(o, If(o +==+ cValue, cFoo, cBar), NullValue)
      }
    }
    "flatMap" - {
      "regular operation" in {
        val q = quote { (o: Option[Option[Int]]) =>
          o.flatMap(i => i.map(j => j + 1))
        }
        new FlattenOptionOperation(StatelessCache.NoCache, AnsiConcat, TraceConfig.Empty)(q.ast.body: Ast) mustEqual
          o +++ c1
        new FlattenOptionOperation(StatelessCache.NoCache, NonAnsiConcat, TraceConfig.Empty)(q.ast.body: Ast) mustEqual
          o +++ c1
      }
      "possible-fallthrough operation" in {
        val q = quote { (o: Option[Option[String]]) =>
          o.flatMap(s => s.map(j => j + "foo"))
        }
        new FlattenOptionOperation(StatelessCache.NoCache, AnsiConcat, TraceConfig.Empty)(q.ast.body: Ast) mustEqual
          o +++ cFoo
        new FlattenOptionOperation(StatelessCache.NoCache, NonAnsiConcat, TraceConfig.Empty)(q.ast.body: Ast) mustEqual
          IfExistElseNull(o, IfExistElseNull(o, o +++ cFoo))
      }
      "never-fallthrough operation" in {
        val q = quote { (o: Option[Option[String]]) =>
          o.flatMap(s => s.map(j => if (j == "value") "foo" else "bar"))
        }
        new FlattenOptionOperation(StatelessCache.NoCache, AnsiConcat, TraceConfig.Empty)(q.ast.body: Ast) mustEqual
          IfExist(o, IfExist(o, If(o +==+ cValue, cFoo, cBar), NullValue), NullValue)
        new FlattenOptionOperation(StatelessCache.NoCache, NonAnsiConcat, TraceConfig.Empty)(q.ast.body: Ast) mustEqual
          IfExist(o, IfExist(o, If(o +==+ cValue, cFoo, cBar), NullValue), NullValue)
      }
    }
    "flatMap row" in {
      val q = quote { (o: Option[Option[Row]]) =>
        o.flatMap(i => i.map(j => j))
      }
      new FlattenOptionOperation(StatelessCache.NoCache, AnsiConcat, TraceConfig.Empty)(q.ast.body: Ast) mustEqual o
    }
    "map" - {
      "regular operation" in {
        val q = quote { (o: Option[Int]) =>
          o.map(i => i + 1)
        }
        new FlattenOptionOperation(StatelessCache.NoCache, AnsiConcat, TraceConfig.Empty)(q.ast.body: Ast) mustEqual
          o +++ c1
        new FlattenOptionOperation(StatelessCache.NoCache, NonAnsiConcat, TraceConfig.Empty)(q.ast.body: Ast) mustEqual
          o +++ c1
      }
      "possible-fallthrough operation" in {
        val q = quote { (o: Option[String]) =>
          o.map(s => s + "foo")
        }
        new FlattenOptionOperation(StatelessCache.NoCache, AnsiConcat, TraceConfig.Empty)(q.ast.body: Ast) mustEqual
          o +++ Constant.auto("foo")
        new FlattenOptionOperation(StatelessCache.NoCache, NonAnsiConcat, TraceConfig.Empty)(q.ast.body: Ast) mustEqual
          IfExistElseNull(o, o +++ cFoo)
      }
      "never-fallthrough operation" in {
        val q = quote { (o: Option[String]) =>
          o.map(s => if (s == "value") "foo" else "bar")
        }
        new FlattenOptionOperation(StatelessCache.NoCache, AnsiConcat, TraceConfig.Empty)(q.ast.body: Ast) mustEqual
          IfExist(o, If(o +==+ cValue, cFoo, cBar), NullValue)
        new FlattenOptionOperation(StatelessCache.NoCache, NonAnsiConcat, TraceConfig.Empty)(q.ast.body: Ast) mustEqual
          IfExist(o, If(o +==+ cValue, cFoo, cBar), NullValue)
      }
    }
    "map row" in {
      val q = quote { (o: Option[Row]) =>
        o.map(i => i)
      }
      new FlattenOptionOperation(StatelessCache.NoCache, AnsiConcat, TraceConfig.Empty)(q.ast.body: Ast) mustEqual o
    }
    "map + getOrElse(true)" in {
      val q = quote { (o: Option[Int]) =>
        o.map(_ < 1).getOrElse(true)
      }
      new FlattenOptionOperation(StatelessCache.NoCache, AnsiConcat, TraceConfig.Empty)(
        q.ast.body: Ast
      ).toString mustEqual "((o < 1) && (o != null)) || (true && (o == null))"
    }
    "map + getOrElse(false)" in {
      val q = quote { (o: Option[Int]) =>
        o.map(_ < 1).getOrElse(false)
      }
      new FlattenOptionOperation(StatelessCache.NoCache, AnsiConcat, TraceConfig.Empty)(
        q.ast.body: Ast
      ).toString mustEqual "((o < 1) && (o != null)) || (false && (o == null))"
    }
    "forall" - {
      "regular operation" in {
        val q = quote { (o: Option[Int]) =>
          o.forall(i => i != 1)
        }
        new FlattenOptionOperation(StatelessCache.NoCache, AnsiConcat, TraceConfig.Empty)(q.ast.body: Ast) mustEqual
          ((o +!=+ c1) +||+ IsNullCheck(o))
        new FlattenOptionOperation(StatelessCache.NoCache, NonAnsiConcat, TraceConfig.Empty)(q.ast.body: Ast) mustEqual
          ((o +!=+ c1) +||+ IsNullCheck(o))
      }
      "possible-fallthrough operation" in {
        val q = quote { (o: Option[String]) =>
          o.forall(s => s + "foo" == "bar")
        }
        new FlattenOptionOperation(StatelessCache.NoCache, AnsiConcat, TraceConfig.Empty)(q.ast.body: Ast) mustEqual
          (((o +++ cFoo) +==+ cBar) +||+ IsNullCheck(o))
        new FlattenOptionOperation(StatelessCache.NoCache, NonAnsiConcat, TraceConfig.Empty)(q.ast.body: Ast) mustEqual
          ((((o +++ cFoo) +==+ cBar) +&&+ IsNotNullCheck(o)) +||+ IsNullCheck(o))
      }
      "never-fallthrough operation" in {
        val q = quote { (o: Option[String]) =>
          o.forall(s => if (s + "foo" == "bar") true else false)
        }
        new FlattenOptionOperation(StatelessCache.NoCache, AnsiConcat, TraceConfig.Empty)(q.ast.body: Ast) mustEqual
          ((If(
            (o +++ cFoo) +==+ cBar,
            Constant.auto(true),
            Constant.auto(false)
          ) +&&+ IsNotNullCheck(o)) +||+ IsNullCheck(o))
        new FlattenOptionOperation(StatelessCache.NoCache, NonAnsiConcat, TraceConfig.Empty)(q.ast.body: Ast) mustEqual
          ((If(
            (o +++ cFoo) +==+ cBar,
            Constant.auto(true),
            Constant.auto(false)
          ) +&&+ IsNotNullCheck(o)) +||+ IsNullCheck(o))
      }
    }
    "map + forall + binop" - {
      "regular operation" in {
        val q = quote { (o: Option[TestEntity]) =>
          o.map(_.i).forall(i => i != 1) && true
        }
        new FlattenOptionOperation(StatelessCache.NoCache, AnsiConcat, TraceConfig.Empty)(q.ast.body: Ast) mustEqual
          ((((Property(o, "i") +!=+ c1)) +||+ IsNullCheck(Property(o, "i"))) +&&+ Constant.auto(true))
      }
      "possible-fallthrough operation" in {
        val q = quote { (o: Option[TestEntity2]) =>
          o.map(_.s).forall(s => s + "foo" == "bar") && true
        }
        new FlattenOptionOperation(StatelessCache.NoCache, AnsiConcat, TraceConfig.Empty)(q.ast.body: Ast) mustEqual
          ((((Property(o, "s") +++ cFoo) +==+ cBar) +||+ IsNullCheck(Property(o, "s")) +&&+ Constant.auto(true)))
        new FlattenOptionOperation(StatelessCache.NoCache, NonAnsiConcat, TraceConfig.Empty)(q.ast.body: Ast) mustEqual
          (((((Property(o, "s") +++ cFoo) +==+ cBar) +&&+ IsNotNullCheck(Property(o, "s"))) +||+ IsNullCheck(Property(o, "s"))) +&&+ Constant.auto(true))
      }
    }
    "exists" - {
      "regular operation" in {
        val q = quote { (o: Option[Int]) =>
          o.exists(i => i > 1)
        }
        new FlattenOptionOperation(StatelessCache.NoCache, AnsiConcat, TraceConfig.Empty)(q.ast.body: Ast) mustEqual
          (o +>+ c1)
        new FlattenOptionOperation(StatelessCache.NoCache, NonAnsiConcat, TraceConfig.Empty)(q.ast.body: Ast) mustEqual
          (o +>+ c1)
      }
      "possible-fallthrough operation" in {
        val q = quote { (o: Option[String]) =>
          o.exists(s => s + "foo" == "bar")
        }
        new FlattenOptionOperation(StatelessCache.NoCache, AnsiConcat, TraceConfig.Empty)(q.ast.body: Ast) mustEqual
          ((o +++ cFoo) +==+ cBar)
        new FlattenOptionOperation(StatelessCache.NoCache, NonAnsiConcat, TraceConfig.Empty)(q.ast.body: Ast) mustEqual
          (((o +++ cFoo) +==+ cBar) +&&+ IsNotNullCheck(o))
      }
      "never-fallthrough operation" in {
        val q = quote { (o: Option[String]) =>
          o.exists(s => if (s + "foo" == "bar") true else false)
        }
        new FlattenOptionOperation(StatelessCache.NoCache, AnsiConcat, TraceConfig.Empty)(q.ast.body: Ast) mustEqual
          (If((o +++ cFoo) +==+ cBar, Constant.auto(true), Constant.auto(false)) +&&+ IsNotNullCheck(o))
        new FlattenOptionOperation(StatelessCache.NoCache, NonAnsiConcat, TraceConfig.Empty)(q.ast.body: Ast) mustEqual
          (If((o +++ cFoo) +==+ cBar, Constant.auto(true), Constant.auto(false)) +&&+ IsNotNullCheck(o))
      }
    }
    "exists row" in {
      val q = quote { (o: Option[Row]) =>
        o.exists(r => r.id != 1)
      }
      new FlattenOptionOperation(StatelessCache.NoCache, AnsiConcat, TraceConfig.Empty)(q.ast.body: Ast) mustEqual (Property(o, "id") +!=+ c1)
    }
    "contains" in {
      val q = quote { (o: Option[Int]) =>
        o.contains(1)
      }
      new FlattenOptionOperation(StatelessCache.NoCache, AnsiConcat, TraceConfig.Empty)(q.ast.body: Ast) mustEqual
        BinaryOperation(Ident("o"), EqualityOperator.`_==`, Constant.auto(1))
    }
  }
}
