package io.getquill.context.orientdb

import com.orientechnologies.orient.core.db.OPartitionedDatabasePool
import com.orientechnologies.orient.core.record.impl.ODocument
import com.orientechnologies.orient.core.sql.query.OSQLQuery
import com.typesafe.scalalogging.Logger
import io.getquill.NamingStrategy
import io.getquill.context.mirror.Row
import io.getquill.context.orientdb.encoding.{ Decoders, Encoders }
import io.getquill.util.Messages.fail
import org.slf4j.LoggerFactory

import scala.collection.mutable.ArrayBuffer
import scala.util.Try

abstract class OrientDBSessionContext[N <: NamingStrategy](
  dbUrl:    String,
  username: String,
  password: String
) extends OrientDBContext[N]
  with Encoders
  with Decoders {

  override type PrepareRow = ArrayBuffer[Any]
  override type ResultRow = ODocument

  override type RunActionReturningResult[T] = Unit
  override type RunBatchActionReturningResult[T] = Unit

  protected val logger: Logger =
    Logger(LoggerFactory.getLogger(classOf[OrientDBSessionContext[_]]))

  protected val session = new OPartitionedDatabasePool(dbUrl, username, password)
  protected val oDatabase = session.acquire()

  protected def prepare() = new ArrayBuffer[Any]()

  override def close(): Unit = {
    oDatabase.close()
    session.close()
  }

  override def probe(orientDBQl: String) =
    Try {
      prepare()
      ()
    }

  def executeActionReturning[O](orientDBQl: String, prepare: OSQLQuery[ODocument] => OSQLQuery[ODocument] = identity, extractor: Row => O, returningColumn: String): Unit = {
    fail("OrientDB doesn't support `returning`.")
  }

  def executeBatchActionReturning[T](groups: List[BatchGroup], extractor: Row => T): Unit = {
    fail("OrientDB doesn't support `returning`.")
  }
}