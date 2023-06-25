package io.getquill.context.cassandra.udt

import io.getquill.Udt
import io.getquill.base.Spec

trait UdtSpec extends Spec {
  case class Name(firstName: String, lastName: Option[String]) extends Udt
  case class Personal(
    number: Int,
    street: String,
    name: Name,
    optName: Option[Name],
    list: List[String],
    sets: Set[Int],
    map: Map[Int, String]
  ) extends Udt
}
