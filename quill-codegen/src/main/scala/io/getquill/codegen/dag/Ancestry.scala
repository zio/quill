package io.getquill.codegen.dag

import com.typesafe.scalalogging.Logger
import io.getquill.codegen.dag.dag.ClassAncestry
import io.getquill.codegen.util.MapExtensions._
import org.slf4j.LoggerFactory

import scala.language.implicitConversions
import scala.reflect.{ClassTag, classTag}

class DagNode(val cls: ClassTag[_], val parent: Option[DagNode])

trait NodeCatalog {
  def lookup(cls: ClassTag[_]): DagNode
}
object DefaultNodeCatalog extends NodeCatalog {

  private val logger = Logger(LoggerFactory.getLogger(this.getClass))

  implicit def nodeToOpt(dagNode: DagNode): Option[DagNode] = Some(dagNode)

  object StringNode extends DagNode(classTag[String], None)

  object BigDecimalNode extends DagNode(classTag[BigDecimal], StringNode)
  object DoubleNode     extends DagNode(classTag[Double], BigDecimalNode)
  object FloatNode      extends DagNode(classTag[Float], DoubleNode)

  object LongNode    extends DagNode(classTag[Long], BigDecimalNode)
  object IntNode     extends DagNode(classTag[Int], LongNode)
  object ShortNode   extends DagNode(classTag[Short], IntNode)
  object ByteNode    extends DagNode(classTag[Byte], ShortNode)
  object BooleanNode extends DagNode(classTag[Boolean], ByteNode)

  object TimestampNode extends DagNode(classTag[java.time.LocalDateTime], StringNode)
  object DateNode      extends DagNode(classTag[java.time.LocalDate], TimestampNode)

  protected[codegen] val nodeCatalogNodes: Seq[DagNode] = Seq(
    StringNode,
    BigDecimalNode,
    DoubleNode,
    FloatNode,
    LongNode,
    IntNode,
    ByteNode,
    ShortNode,
    BooleanNode,
    TimestampNode,
    DateNode
  )

  override def lookup(cls: ClassTag[_]): DagNode = nodeCatalogNodes
    .find(_.cls == cls)
    .getOrElse {
      logger.warn(s"Could not find type hierarchy node for: ${cls} Must assume it's a string")
      StringNode
    }
}

package object dag {
  type ClassAncestry = (ClassTag[_], ClassTag[_]) => ClassTag[_]
}

class CatalogBasedAncestry(ancestryCatalog: NodeCatalog = DefaultNodeCatalog) extends ClassAncestry {

  def apply(one: ClassTag[_], two: ClassTag[_]): ClassTag[_] = {

    def getAncestry(node: DagNode): List[DagNode] = node.parent match {
      case Some(parent) => node :: getAncestry(parent)
      case None         => node :: Nil
    }

    def commonAncestry = {
      val oneAncestry = getAncestry(ancestryCatalog.lookup(one))
      val twoAncestry = getAncestry(ancestryCatalog.lookup(two))

      val (node, _) =
        oneAncestry.zipWithIndex.toMap
          .zipOnKeys(twoAncestry.zipWithIndex.toMap)
          .collect { case (key, (Some(i), Some(j))) => (key, i + j) }
          .toList
          .sortBy { case (node, order) => order }
          .head

      node.cls
    }

    // If the two nodes are exactly the same thing, just return the type. Otherwise look up the DAG.
    if (one == two)
      one
    else
      commonAncestry
  }
}
