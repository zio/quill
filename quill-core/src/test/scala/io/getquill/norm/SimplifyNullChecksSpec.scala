package io.getquill.norm

import io.getquill.ast._
import io.getquill.ast.Implicits._
import io.getquill.base.Spec
import io.getquill.norm.EqualityBehavior.{AnsiEquality, NonAnsiEquality}
import io.getquill.MirrorContexts.testContext.{quote, unquote}
import io.getquill.MirrorContexts.testContext.extras._
import io.getquill.StatelessCache

class SimplifyNullChecksSpec extends Spec {

  // remove the === matcher from scalatest so that we can test === in Context.extra
  override def convertToEqualizer[T](left: T): Equalizer[T] = new Equalizer(left)

  val ia = Ident("a")
  val ib = Ident("b")
  val it = Ident("t")
  val ca = Constant.auto("a")

  val SimplifyNullChecksAnsi    = new SimplifyNullChecks(StatelessCache.NoCache, AnsiEquality)
  val SimplifyNullChecksNonAnsi = new SimplifyNullChecks(StatelessCache.NoCache, NonAnsiEquality)

  "center rule must" - {
    "apply when conditionals same" in {
      SimplifyNullChecksAnsi(
        IfExist(
          IfExistElseNull(ia, it),
          IfExistElseNull(ia, it),
          Ident("o")
        )
      ) mustEqual If(
        IsNotNullCheck(Ident("a")) +&&+ IsNotNullCheck(Ident("t")),
        Ident("t"),
        Ident("o")
      )
    }

    "apply left rule" in {
      SimplifyNullChecksAnsi(
        IfExist(IfExistElseNull(ia, ib), ca, it)
      ) mustEqual If(IsNotNullCheck(ia) +&&+ IsNotNullCheck(ib), ca, it)
    }

    "apply right rule" in {
      SimplifyNullChecksAnsi(
        IfExistElseNull(ia, IfExistElseNull(ib, it))
      ) mustEqual If(IsNotNullCheck(ia) +&&+ IsNotNullCheck(ib), it, NullValue)
    }

    "reduces when Option == Option(constant)" in {
      val q = quote { (a: Option[Int]) =>
        a == Option(1)
      }
      SimplifyNullChecksNonAnsi(quote(unquote(q)).ast.body) mustEqual (OptionIsDefined(ia) +&&+ (ia +==+ OptionApply(
        Constant.auto(1)
      )))
    }

    "reduces when Option(constant) == Option" in {
      val q = quote { (a: Option[Int]) =>
        Option(1) == a
      }
      SimplifyNullChecksNonAnsi(quote(unquote(q)).ast.body) mustEqual (OptionIsDefined(ia) +&&+ (OptionApply(
        Constant.auto(1)
      ) +==+ ia))
    }

    "reduces when Option != Option(constant)" in {
      val q = quote { (a: Option[Int]) =>
        a != Option(1)
      }
      SimplifyNullChecksAnsi(quote(unquote(q)).ast.body) mustEqual (OptionIsEmpty(ia) +||+ (ia +!=+ OptionApply(
        Constant.auto(1)
      )))
    }

    "reduces when Option(constant) != Option" in {
      val q = quote { (a: Option[Int]) =>
        Option(1) != a
      }
      SimplifyNullChecksAnsi(quote(unquote(q)).ast.body) mustEqual (OptionIsEmpty(ia) +||+ (OptionApply(
        Constant.auto(1)
      ) +!=+ ia))
    }

    "with ansi enabled" - {
      "Ignores regular comparison - Int/Int" in {
        val q = quote { (a: Int, b: Int) =>
          a === b
        }
        SimplifyNullChecksAnsi(quote(unquote(q)).ast.body) mustEqual BinaryOperation(
          Ident("a"),
          EqualityOperator.`_==`,
          Ident("b")
        )
      }
      "reduces when Option/Option" in {
        val q = quote { (a: Option[Int], b: Option[Int]) =>
          a === b
        }
        SimplifyNullChecksAnsi(quote(unquote(q)).ast.body) mustEqual (ia +==+ ib)
      }
      "succeeds when Option/T" in {
        val q = quote { (a: Option[Int], b: Int) =>
          a === b
        }
        SimplifyNullChecksAnsi(quote(unquote(q)).ast.body) mustEqual (ia +==+ ib)
      }
      "succeeds when T/Option" in {
        val q = quote { (a: Int, b: Option[Int]) =>
          a === b
        }
        SimplifyNullChecksAnsi(quote(unquote(q)).ast.body) mustEqual (ia +==+ ib)
      }
    }

    "without ansi enabled" - {
      "Ignores regular comparison - Int/Int" in {
        val q = quote { (a: Int, b: Int) =>
          a === b
        }
        SimplifyNullChecksNonAnsi(quote(unquote(q)).ast.body) mustEqual BinaryOperation(
          Ident("a"),
          EqualityOperator.`_==`,
          Ident("b")
        )
      }
      "reduces when Option/Option" in {
        val q = quote { (a: Option[Int], b: Option[Int]) =>
          a === b
        }
        SimplifyNullChecksNonAnsi(quote(unquote(q)).ast.body) mustEqual OptionIsDefined(ia) +&&+ OptionIsDefined(
          ib
        ) +&&+ (ia +==+ ib)
      }
      "succeeds when Option/T" in {
        val q = quote { (a: Option[Int], b: Int) =>
          a === b
        }
        SimplifyNullChecksNonAnsi(quote(unquote(q)).ast.body) mustEqual OptionIsDefined(ia) +&&+ (ia +==+ ib)
      }
      "succeeds when T/Option" in {
        val q = quote { (a: Int, b: Option[Int]) =>
          a === b
        }
        SimplifyNullChecksNonAnsi(quote(unquote(q)).ast.body) mustEqual OptionIsDefined(ib) +&&+ (ia +==+ ib)
      }
    }
  }
}
