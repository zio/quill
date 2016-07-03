package io.getquill.dsl

import scala.language.experimental.macros
import scala.language.implicitConversions
import scala.reflect.macros.whitebox.Context

import io.getquill.ast.Ast
import io.getquill.quotation.NonQuotedException
import io.getquill.quotation.Quotation

private[dsl] trait QuotationDsl {

  trait Quoted[+T] {
    def ast: Ast
  }

  def lift[T](v: T): T = NonQuotedException()

  def quote[T](body: Quoted[T]): Quoted[T] = macro QuotationMacro.doubleQuote[T]
  def quote[T1, R](func: T1 => Quoted[R]): Quoted[T1 => R] = macro QuotationMacro.quotedFunctionBody
  def quote[T1, T2, R](func: (T1, T2) => Quoted[R]): Quoted[(T1, T2) => R] = macro QuotationMacro.quotedFunctionBody
  def quote[T1, T2, T3, R](func: (T1, T2, T3) => Quoted[R]): Quoted[(T1, T2, T3) => R] = macro QuotationMacro.quotedFunctionBody
  def quote[T1, T2, T3, T4, R](func: (T1, T2, T3, T4) => Quoted[R]): Quoted[(T1, T2, T3, T4) => R] = macro QuotationMacro.quotedFunctionBody
  def quote[T1, T2, T3, T4, T5, R](func: (T1, T2, T3, T4, T5) => Quoted[R]): Quoted[(T1, T2, T3, T4, T5) => R] = macro QuotationMacro.quotedFunctionBody
  def quote[T1, T2, T3, T4, T5, T6, R](func: (T1, T2, T3, T4, T5, T6) => Quoted[R]): Quoted[(T1, T2, T3, T4, T5, T6) => R] = macro QuotationMacro.quotedFunctionBody
  def quote[T1, T2, T3, T4, T5, T6, T7, R](func: (T1, T2, T3, T4, T5, T6, T7) => Quoted[R]): Quoted[(T1, T2, T3, T4, T5, T6, T7) => R] = macro QuotationMacro.quotedFunctionBody
  def quote[T1, T2, T3, T4, T5, T6, T7, T8, R](func: (T1, T2, T3, T4, T5, T6, T7, T8) => Quoted[R]): Quoted[(T1, T2, T3, T4, T5, T6, T7, T8) => R] = macro QuotationMacro.quotedFunctionBody
  def quote[T1, T2, T3, T4, T5, T6, T7, T8, T9, R](func: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => Quoted[R]): Quoted[(T1, T2, T3, T4, T5, T6, T7, T8, T9) => R] = macro QuotationMacro.quotedFunctionBody
  def quote[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R](func: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => Quoted[R]): Quoted[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => R] = macro QuotationMacro.quotedFunctionBody

  implicit def quote[T](body: T): Quoted[T] = macro QuotationMacro.quote[T]
  implicit def unquote[T](quoted: Quoted[T]): T = NonQuotedException()
}

private[dsl] class QuotationMacro(val c: Context) extends Quotation
