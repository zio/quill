package io.getquill.norm

import io.getquill.ast.Renameable.Fixed
import io.getquill.ast.Visibility.Visible
import io.getquill.ast._
import io.getquill.base.Spec
import io.getquill.quat.Quat

class BetaReductionSpec extends Spec {

  "simplifies the ast by applying functions" - {
    "tuple field" in {
      val ast: Ast = Property(Tuple(List(Ident("a"))), "_1")
      BetaReduction(ast) mustEqual Ident("a")
    }
    "tuple field - fixed property" in {
      val ast: Ast = Property.Opinionated(Tuple(List(Ident("a"))), "_1", Fixed, Visible)
      BetaReduction(ast) mustEqual Ident("a")
    }
    "caseclass field" in {
      val ast: Ast = Property(CaseClass("CC", List(("foo", Ident("a")))), "foo")
      BetaReduction(ast) mustEqual Ident("a")
    }
    "caseclass field - fixed property" in {
      val ast: Ast = Property.Opinionated(CaseClass("CC", List(("foo", Ident("a")))), "foo", Fixed, Visible)
      BetaReduction(ast) mustEqual Ident("a")
    }
    "function apply" in {
      val function = Function(List(Ident("a")), Ident("a"))
      val ast: Ast = FunctionApply(function, List(Ident("b")))
      BetaReduction(ast) mustEqual Ident("b")
    }
  }

  "replaces identifiers by actuals" - {
    "ident" in {
      val ast: Ast = Ident("a")
      BetaReduction(ast, Ident("a") -> Ident("a'")) mustEqual
        Ident("a'")
    }
    "with OnConflict.Excluded" in {
      val ast: Ast = OnConflict.Excluded(Ident("a"))
      BetaReduction(ast, Ident("a") -> Ident("a'")) mustEqual
        OnConflict.Excluded(Ident("a'"))
    }
    "with OnConflict.Existing" in {
      val ast: Ast = OnConflict.Existing(Ident("a"))
      BetaReduction(ast, Ident("a") -> Ident("a'")) mustEqual
        OnConflict.Existing(Ident("a'"))
    }
    "with inline" - {
      val entity = Entity("a", Nil, QEP)
      // Entity Idents
      val (aE, bE, cE, dE) = (Ident("a", QEP), Ident("b", QEP), Ident("c", QEP), Ident("d", QEP))
      // Value Idents
      val (a, b, c, d) = (Ident("a", QV), Ident("b", QV), Ident("c", QV), Ident("d", QV))
      val (c1, c2, c3) = (Constant.auto(1), Constant.auto(2), Constant.auto(3))

      "top level block" in {
        val block = Block(
          List(
            Val(aE, entity),
            Val(bE, aE),
            Map(bE, dE, c1)
          )
        )
        BetaReduction.AllowEmpty(block) mustEqual Map(entity, dE, c1)
      }
      "nested blocks" in {
        val inner = Block(
          List(
            Val(aE, entity),
            Val(b, c2),
            Val(c, c3),
            Tuple(List(aE, bE, cE))
          )
        )
        val outer = Block(
          List(
            Val(aE, inner),
            Val(bE, aE),
            Val(cE, bE),
            cE
          )
        )
        BetaReduction.AllowEmpty(outer) mustEqual Tuple(List(entity, c2, c3))
      }
      "nested blocks caseclass" in {
        val inner = Block(
          List(
            Val(aE, entity),
            Val(b, c2),
            Val(c, c3),
            CaseClass("CC", List(("foo", aE), ("bar", bE), ("baz", cE)))
          )
        )
        val outer = Block(
          List(
            Val(aE, inner),
            Val(bE, aE),
            Val(cE, bE),
            cE
          )
        )
        BetaReduction.AllowEmpty(outer) mustEqual CaseClass("CC", List(("foo", entity), ("bar", c2), ("baz", c3)))
      }
    }
    "avoids replacing idents of an outer scope" - {
      "filter" in {
        val ast: Ast = Filter(Ident("a"), Ident("b"), Ident("b"))
        BetaReduction(ast, Ident("b") -> Ident("b'")) mustEqual ast
      }
      "map" in {
        val ast: Ast = Map(Ident("a"), Ident("b"), Ident("b"))
        BetaReduction(ast, Ident("b") -> Ident("b'")) mustEqual ast
      }
      "flatMap" in {
        val ast: Ast = FlatMap(Ident("a"), Ident("b"), Ident("b"))
        BetaReduction(ast, Ident("b") -> Ident("b'")) mustEqual ast
      }
      "concatMap" in {
        val ast: Ast = ConcatMap(Ident("a"), Ident("b"), Ident("b"))
        BetaReduction(ast, Ident("b") -> Ident("b'")) mustEqual ast
      }
      "sortBy" in {
        val ast: Ast = SortBy(Ident("a"), Ident("b"), Ident("b"), AscNullsFirst)
        BetaReduction(ast, Ident("b") -> Ident("b'")) mustEqual ast
      }
      "groupBy" in {
        val ast: Ast = GroupBy(Ident("a"), Ident("b"), Ident("b"))
        BetaReduction(ast, Ident("b") -> Ident("b'")) mustEqual ast
      }
      "outer join" in {
        val ast: Ast =
          Join(LeftJoin, Ident("a"), Ident("b"), Ident("c"), Ident("d"), Tuple(List(Ident("c"), Ident("d"))))
        BetaReduction(ast, Ident("c") -> Ident("c'"), Ident("d") -> Ident("d'")) mustEqual ast
      }
      "option operation" - {
        "flatMap - Unchecked" in {
          val ast: Ast = OptionTableFlatMap(Ident("a"), Ident("b"), Ident("b"))
          BetaReduction(ast, Ident("b") -> Ident("b'")) mustEqual ast
        }
        "map - Unchecked" in {
          val ast: Ast = OptionTableMap(Ident("a"), Ident("b"), Ident("b"))
          BetaReduction(ast, Ident("b") -> Ident("b'")) mustEqual ast
        }
        "flatMap" in {
          val ast: Ast = OptionFlatMap(Ident("a"), Ident("b"), Ident("b"))
          BetaReduction(ast, Ident("b") -> Ident("b'")) mustEqual ast
        }
        "map" in {
          val ast: Ast = OptionMap(Ident("a"), Ident("b"), Ident("b"))
          BetaReduction(ast, Ident("b") -> Ident("b'")) mustEqual ast
        }
        "forall" in {
          val ast: Ast = OptionForall(Ident("a"), Ident("b"), Ident("b"))
          BetaReduction(ast, Ident("b") -> Ident("b'")) mustEqual ast
        }
        "forall - Unchecked" in {
          val ast: Ast = OptionTableForall(Ident("a"), Ident("b"), Ident("b"))
          BetaReduction(ast, Ident("b") -> Ident("b'")) mustEqual ast
        }
        "exists" in {
          val ast: Ast = OptionExists(Ident("a"), Ident("b"), Ident("b"))
          BetaReduction(ast, Ident("b") -> Ident("b'")) mustEqual ast
        }
        "exists - Unchecked" in {
          val ast: Ast = OptionTableExists(Ident("a"), Ident("b"), Ident("b"))
          BetaReduction(ast, Ident("b") -> Ident("b'")) mustEqual ast
        }
      }
    }
  }

  "doesn't shadow identifiers" - {
    "function apply" in {
      val ast: Ast = FunctionApply(
        Function(List(Ident("a"), Ident("b")), BinaryOperation(Ident("a"), NumericOperator.`/`, Ident("b"))),
        List(Ident("b"), Ident("a"))
      )
      BetaReduction(ast) mustEqual BinaryOperation(Ident("b"), NumericOperator.`/`, Ident("a"))
    }
    "nested function apply" in {
      // (b) => a/b
      val f1       = Function(List(Ident("b")), BinaryOperation(Ident("a"), NumericOperator.`/`, Ident("b")))
      // (a) => (b) => a/b
      val f2       = Function(List(Ident("a")), f1)
      // ((a) => (b) => a/b).apply(b).apply(a) ->
      //   (tmp_b) => a/tmp_b ->
      //       (a) => tmp_a/tmp_b ->
      //       tmp_a/a
      //     b/a
      val ast: Ast = FunctionApply(FunctionApply(f2, List(Ident("b"))), List(Ident("a")))
      BetaReduction(ast) mustEqual BinaryOperation(Ident("b"), NumericOperator.`/`, Ident("a"))
    }
  }

  "treats duplicate aliases normally" in {
    val property: Ast = Property(Tuple(List(Ident("a"), Ident("a"))), "_1")
    BetaReduction(property, Ident("a") -> Ident("a'")) mustEqual
      Ident("a'")
  }

  "reapplies the beta reduction if the structure changes" in {
    val quat     = Quat.LeafTuple(1)
    val ast: Ast = Property(Ident("a", quat), "_1")
    BetaReduction(ast, Ident("a", quat) -> Tuple(List(Ident("a'")))) mustEqual
      Ident("a'")
  }

  "reapplies the beta reduction if the structure changes caseclass" in {
    val quat     = Quat.LeafProduct("foo")
    val ast: Ast = Property(Ident("a", quat), "foo")
    BetaReduction(ast, Ident("a", quat) -> CaseClass("CC", List(("foo", Ident("a'"))))) mustEqual
      Ident("a'")
  }

  "applies reduction only once" in {
    val ast: Ast = Ident("a")
    BetaReduction(ast, Ident("a") -> Ident("b"), Ident("b") -> Ident("c")) mustEqual
      Ident("b")
  }
}
