package io.getquill.quotation

import scala.reflect.macros.whitebox.Context

import io.getquill.ast._
import io.getquill.dsl.CoreDsl

trait Liftables {
  val c: Context
  import c.universe.{ Ident => _, Constant => _, Function => _, If => _, Block => _, _ }

  private val pack = q"io.getquill.ast"

  implicit val astLiftable: Liftable[Ast] = Liftable[Ast] {
    case ast: Query => queryLiftable(ast)
    case ast: Action => actionLiftable(ast)
    case ast: Value => valueLiftable(ast)
    case ast: Ident => identLiftable(ast)
    case ast: Ordering => orderingLiftable(ast)
    case ast: Lift => liftLiftable(ast)
    case ast: Assignment => assignmentLiftable(ast)
    case Val(name, body) => q"$pack.Val($name, $body)"
    case Block(statements) => q"$pack.Block($statements)"
    case Property(a, b) => q"$pack.Property($a, $b)"
    case Function(a, b) => q"$pack.Function($a, $b)"
    case FunctionApply(a, b) => q"$pack.FunctionApply($a, $b)"
    case BinaryOperation(a, b, c) => q"$pack.BinaryOperation($a, $b, $c)"
    case UnaryOperation(a, b) => q"$pack.UnaryOperation($a, $b)"
    case Infix(a, b) => q"$pack.Infix($a, $b)"
    case OptionOperation(a, b, c, d) => q"$pack.OptionOperation($a, $b, $c, $d)"
    case If(a, b, c) => q"$pack.If($a, $b, $c)"
    case Dynamic(tree: Tree) if (tree.tpe <:< c.weakTypeOf[CoreDsl#Quoted[Any]]) => q"$tree.ast"
    case Dynamic(tree: Tree) => q"$pack.Constant($tree)"
    case QuotedReference(tree: Tree, ast) => q"$ast"
  }

  implicit val optionOperationTypeLiftable: Liftable[OptionOperationType] = Liftable[OptionOperationType] {
    case OptionMap    => q"$pack.OptionMap"
    case OptionForall => q"$pack.OptionForall"
    case OptionExists => q"$pack.OptionExists"
  }

  implicit val binaryOperatorLiftable: Liftable[BinaryOperator] = Liftable[BinaryOperator] {
    case EqualityOperator.`==`  => q"$pack.EqualityOperator.`==`"
    case EqualityOperator.`!=`  => q"$pack.EqualityOperator.`!=`"
    case BooleanOperator.`&&`   => q"$pack.BooleanOperator.`&&`"
    case BooleanOperator.`||`   => q"$pack.BooleanOperator.`||`"
    case StringOperator.`+`     => q"$pack.StringOperator.`+`"
    case NumericOperator.`-`    => q"$pack.NumericOperator.`-`"
    case NumericOperator.`+`    => q"$pack.NumericOperator.`+`"
    case NumericOperator.`*`    => q"$pack.NumericOperator.`*`"
    case NumericOperator.`>`    => q"$pack.NumericOperator.`>`"
    case NumericOperator.`>=`   => q"$pack.NumericOperator.`>=`"
    case NumericOperator.`<`    => q"$pack.NumericOperator.`<`"
    case NumericOperator.`<=`   => q"$pack.NumericOperator.`<=`"
    case NumericOperator.`/`    => q"$pack.NumericOperator.`/`"
    case NumericOperator.`%`    => q"$pack.NumericOperator.`%`"
    case SetOperator.`contains` => q"$pack.SetOperator.`contains`"
  }

  implicit val unaryOperatorLiftable: Liftable[UnaryOperator] = Liftable[UnaryOperator] {
    case NumericOperator.`-`          => q"$pack.NumericOperator.`-`"
    case BooleanOperator.`!`          => q"$pack.BooleanOperator.`!`"
    case StringOperator.`toUpperCase` => q"$pack.StringOperator.`toUpperCase`"
    case StringOperator.`toLowerCase` => q"$pack.StringOperator.`toLowerCase`"
    case StringOperator.`toLong`      => q"$pack.StringOperator.`toLong`"
    case StringOperator.`toInt`       => q"$pack.StringOperator.`toInt`"
    case SetOperator.`nonEmpty`       => q"$pack.SetOperator.`nonEmpty`"
    case SetOperator.`isEmpty`        => q"$pack.SetOperator.`isEmpty`"
  }

  implicit val aggregationOperatorLiftable: Liftable[AggregationOperator] = Liftable[AggregationOperator] {
    case AggregationOperator.`min`  => q"$pack.AggregationOperator.`min`"
    case AggregationOperator.`max`  => q"$pack.AggregationOperator.`max`"
    case AggregationOperator.`avg`  => q"$pack.AggregationOperator.`avg`"
    case AggregationOperator.`sum`  => q"$pack.AggregationOperator.`sum`"
    case AggregationOperator.`size` => q"$pack.AggregationOperator.`size`"
  }

  implicit val queryLiftable: Liftable[Query] = Liftable[Query] {
    case Entity(a, b)           => q"$pack.Entity($a, $b)"
    case Filter(a, b, c)        => q"$pack.Filter($a, $b, $c)"
    case Map(a, b, c)           => q"$pack.Map($a, $b, $c)"
    case FlatMap(a, b, c)       => q"$pack.FlatMap($a, $b, $c)"
    case SortBy(a, b, c, d)     => q"$pack.SortBy($a, $b, $c, $d)"
    case GroupBy(a, b, c)       => q"$pack.GroupBy($a, $b, $c)"
    case Aggregation(a, b)      => q"$pack.Aggregation($a, $b)"
    case Take(a, b)             => q"$pack.Take($a, $b)"
    case Drop(a, b)             => q"$pack.Drop($a, $b)"
    case Union(a, b)            => q"$pack.Union($a, $b)"
    case UnionAll(a, b)         => q"$pack.UnionAll($a, $b)"
    case Join(a, b, c, d, e, f) => q"$pack.Join($a, $b, $c, $d, $e, $f)"
    case FlatJoin(a, b, c, d)   => q"$pack.FlatJoin($a, $b, $c, $d)"
    case Distinct(a)            => q"$pack.Distinct($a)"
    case Nested(a)              => q"$pack.Nested($a)"
  }

  implicit val propertyAliasLiftable: Liftable[PropertyAlias] = Liftable[PropertyAlias] {
    case PropertyAlias(a, b) => q"$pack.PropertyAlias($a, $b)"
  }

  implicit val orderingLiftable: Liftable[Ordering] = Liftable[Ordering] {
    case TupleOrdering(elems) => q"$pack.TupleOrdering($elems)"
    case Asc                  => q"$pack.Asc"
    case Desc                 => q"$pack.Desc"
    case AscNullsFirst        => q"$pack.AscNullsFirst"
    case DescNullsFirst       => q"$pack.DescNullsFirst"
    case AscNullsLast         => q"$pack.AscNullsLast"
    case DescNullsLast        => q"$pack.DescNullsLast"
  }

  implicit val joinTypeLiftable: Liftable[JoinType] = Liftable[JoinType] {
    case InnerJoin => q"$pack.InnerJoin"
    case LeftJoin  => q"$pack.LeftJoin"
    case RightJoin => q"$pack.RightJoin"
    case FullJoin  => q"$pack.FullJoin"
  }

  implicit val actionLiftable: Liftable[Action] = Liftable[Action] {
    case Update(a, b)       => q"$pack.Update($a, $b)"
    case Insert(a, b)       => q"$pack.Insert($a, $b)"
    case Delete(a)          => q"$pack.Delete($a)"
    case Returning(a, b, c) => q"$pack.Returning($a, $b, $c)"
    case Foreach(a, b, c)   => q"$pack.Foreach($a, $b, $c)"
  }

  implicit val assignmentLiftable: Liftable[Assignment] = Liftable[Assignment] {
    case Assignment(a, b, c) => q"$pack.Assignment($a, $b, $c)"
  }

  implicit val valueLiftable: Liftable[Value] = Liftable[Value] {
    case NullValue   => q"$pack.NullValue"
    case Constant(a) => q"$pack.Constant(${Literal(c.universe.Constant(a))})"
    case Tuple(a)    => q"$pack.Tuple($a)"
  }
  implicit val identLiftable: Liftable[Ident] = Liftable[Ident] {
    case Ident(a) => q"$pack.Ident($a)"
  }

  implicit val liftLiftable: Liftable[Lift] = Liftable[Lift] {
    case ScalarValueLift(a, b: Tree, c: Tree) => q"$pack.ScalarValueLift($a, $b, $c)"
    case CaseClassValueLift(a, b: Tree)       => q"$pack.CaseClassValueLift($a, $b)"
    case ScalarQueryLift(a, b: Tree, c: Tree) => q"$pack.ScalarQueryLift($a, $b, $c)"
    case CaseClassQueryLift(a, b: Tree)       => q"$pack.CaseClassQueryLift($a, $b)"
  }
}
