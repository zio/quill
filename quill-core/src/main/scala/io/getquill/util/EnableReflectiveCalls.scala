package io.getquill.util

import scala.reflect.macros.blackbox.Context
import scala.reflect.api.Trees

object EnableReflectiveCalls {

  def apply(c: Context): List[c.universe.SymTree with c.universe.SymTreeApi with c.universe.Tree] = {
    import c.universe._
    q"import _root_.scala.language.reflectiveCalls" ::
      q"Nil.asInstanceOf[{ def size }].size" ::
      Nil
  }
}