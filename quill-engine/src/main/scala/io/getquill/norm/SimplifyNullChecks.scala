package io.getquill.norm

import io.getquill.{HasStatelessCache, StatelessCache}
import io.getquill.ast._
import io.getquill.ast.Implicits._
import io.getquill.norm.EqualityBehavior.AnsiEquality

/**
 * Due to the introduction of null checks in `map`, `flatMap`, and `exists`, in
 * `FlattenOptionOperation` in order to resolve #1053, as well as to support
 * non-ansi compliant string concatenation as outlined in #1295, large
 * conditional composites became common. For example: <code><pre> case class
 * Holder(value:Option[String])
 *
 * // The following statement query[Holder].map(h => h.value.map(_ + "foo")) //
 * Will yield the following result SELECT CASE WHEN h.value IS NOT NULL THEN
 * h.value || 'foo' ELSE null END FROM Holder h </pre></code> Now, let's add a
 * <code>getOrElse</code> statement to the clause that requires an additional
 * wrapped null check. We cannot rely on there being a <code>map</code> call
 * beforehand since we could be reading <code>value</code> as a nullable field
 * directly from the database). <code><pre> // The following statement
 * query[Holder].map(h => h.value.map(_ + "foo").getOrElse("bar")) // Yields the
 * following result: SELECT CASE WHEN CASE WHEN h.value IS NOT NULL THEN h.value
 * \|| 'foo' ELSE null END IS NOT NULL THEN CASE WHEN h.value IS NOT NULL THEN
 * h.value || 'foo' ELSE null END ELSE 'bar' END FROM Holder h </pre></code>
 * This of course is highly redundant and can be reduced to simply: <code><pre>
 * SELECT CASE WHEN h.value IS NOT NULL AND (h.value || 'foo') IS NOT NULL THEN
 * h.value || 'foo' ELSE 'bar' END FROM Holder h </pre></code> This reduction is
 * done by the "Center Rule." There are some other simplification rules as well.
 * Note how we are force to null-check both `h.value` as well as `(h.value ||
 * 'foo')` because a user may use `Option[T].flatMap` and explicitly transform a
 * particular value to `null`.
 */
class SimplifyNullChecks(val cache: StatelessCache, equalityBehavior: EqualityBehavior) extends StatelessTransformer with HasStatelessCache {

  override def apply(ast: Ast): Ast = cached(ast) {
    ast match {

      // Center rule
      case IfExist(
            IfExistElseNull(condA, thenA),
            IfExistElseNull(condB, thenB),
            otherwise
          ) if (condA == condB && thenA == thenB) =>
        apply(If(IsNotNullCheck(condA) +&&+ IsNotNullCheck(thenA), thenA, otherwise))

      // Left hand rule
      case IfExist(IfExistElseNull(check, affirm), value, otherwise) =>
        apply(If(IsNotNullCheck(check) +&&+ IsNotNullCheck(affirm), value, otherwise))

      // Right hand rule
      case IfExistElseNull(cond, IfExistElseNull(innerCond, innerThen)) =>
        apply(If(IsNotNullCheck(cond) +&&+ IsNotNullCheck(innerCond), innerThen, NullValue))

      case OptionIsDefined(Optional(a)) +&&+ OptionIsDefined(Optional(b)) +&&+ (exp @ (Optional(a1) `== or !=` Optional(
            b1
          ))) if (a == a1 && b == b1 && equalityBehavior == AnsiEquality) =>
        apply(exp)

      case OptionIsDefined(Optional(a)) +&&+ (exp @ (Optional(a1) `== or !=` Optional(_)))
          if (a == a1 && equalityBehavior == AnsiEquality) =>
        apply(exp)
      case OptionIsDefined(Optional(b)) +&&+ (exp @ (Optional(_) `== or !=` Optional(b1)))
          if (b == b1 && equalityBehavior == AnsiEquality) =>
        apply(exp)

      case (left +&&+ OptionIsEmpty(Optional(Constant(_, _)))) +||+ other  => apply(other)
      case (OptionIsEmpty(Optional(Constant(_, _))) +&&+ right) +||+ other => apply(other)
      case other +||+ (left +&&+ OptionIsEmpty(Optional(Constant(_, _))))  => apply(other)
      case other +||+ (OptionIsEmpty(Optional(Constant(_, _))) +&&+ right) => apply(other)

      case (left +&&+ OptionIsDefined(Optional(Constant(_, _))))  => apply(left)
      case (OptionIsDefined(Optional(Constant(_, _))) +&&+ right) => apply(right)
      case (left +||+ OptionIsEmpty(Optional(Constant(_, _))))    => apply(left)
      case (OptionIsEmpty(OptionSome(Optional(_))) +||+ right)    => apply(right)

      case other =>
        super.apply(other)
    }
  }

  object `== or !=` {
    def unapply(ast: Ast): Option[(Ast, Ast)] = ast match {
      case a +==+ b => Some((a, b))
      case a +!=+ b => Some((a, b))
      case _        => None
    }
  }

  /**
   * Simple extractor that looks inside of an optional values to see if the
   * thing inside can be pulled out. If not, it just returns whatever element it
   * can find.
   */
  object Optional {
    def unapply(a: Ast): Option[Ast] = a match {
      case OptionApply(value) => Some(value)
      case OptionSome(value)  => Some(value)
      case value              => Some(value)
    }
  }
}
