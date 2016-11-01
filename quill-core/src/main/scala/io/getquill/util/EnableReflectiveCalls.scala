package io.getquill.util

import scala.reflect.macros.blackbox.Context

object EnableReflectiveCalls {

  def apply(c: Context) = {
    import c.universe._
    q"import scala.language.reflectiveCalls" ::
      q"Nil.asInstanceOf[{ def size }].size" ::
      Nil
  }
}