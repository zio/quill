package io.getquill.context.spark

import org.apache.spark.sql.Dataset

sealed trait Binding

final case class DatasetBinding[T](ds: Dataset[T]) extends Binding

final case class ValueBinding(str: String) extends Binding
