package io.getquill

import com.orientechnologies.orient.core.record.impl.ODocument
import com.orientechnologies.orient.core.sql.OCommandSQL
import com.orientechnologies.orient.core.sql.query.OSQLSynchQuery
import com.typesafe.config.Config
import io.getquill.context.orientdb.OrientDBSessionContext
import io.getquill.util.{ ContextLogger, LoadConfig }

import scala.collection.JavaConverters._

class OrientDBSyncContext[N <: NamingStrategy](
  dbUrl:    String,
  username: String,
  password: String
) extends OrientDBSessionContext[N](dbUrl, username, password) {

  def this(config: OrientDBContextConfig) = this(config.dbUrl, config.username, config.password)
  def this(config: Config) = this(OrientDBContextConfig(config))
  def this(configPrefix: String) = this(LoadConfig(configPrefix))

  override type RunQueryResult[T] = List[T]
  override type RunQuerySingleResult[T] = T
  override type RunQueryHeadOptionResult[T] = Option[T]
  override type RunActionResult = Unit
  override type RunBatchActionResult = Unit

  private val logger = ContextLogger(classOf[OrientDBSyncContext[_]])

  def executeQuery[T](orientQl: String, prepare: Prepare = identityPrepare, extractor: Extractor[T] = identityExtractor): List[T] = {
    val (params, objects) = prepare(super.prepare())
    logger.logQuery(orientQl, params)
    oDatabase.query[java.util.List[ODocument]](new OSQLSynchQuery[ODocument](checkInFilter(orientQl, objects.size)), objects.asJava).asScala.map(extractor).toList
  }

  def executeQuerySingle[T](orientQl: String, prepare: Prepare = identityPrepare, extractor: Extractor[T] = identityExtractor): T =
    handleSingleResult(executeQuery(orientQl, prepare, extractor))

  def executeQueryHeadOption[T](orientQl: String, prepare: Prepare = identityPrepare, extractor: Extractor[T] = identityExtractor): Option[T] =
    handleHeadOptionResult(executeQuery(orientQl, prepare, extractor))

  def executeAction[T](orientQl: String, prepare: Prepare = identityPrepare): Unit = {
    val (params, objects) = prepare(super.prepare())
    logger.logQuery(orientQl, params)
    oDatabase.command(new OCommandSQL(orientQl)).execute(objects.toArray)
  }

  def executeBatchAction[T](groups: List[BatchGroup]): Unit = {
    groups.foreach {
      case BatchGroup(orientQl, prepare) =>
        prepare.foreach(executeAction(orientQl, _))
    }
  }

  private def checkInFilter(orientQl: String, noOfLifts: Int): String = {
    // Issue with OrientDB IN: https://stackoverflow.com/questions/34391006/orientdb-passing-an-array-to-a-query-using-in-on-an-otype-linklist-field
    val orientInFilterString = s"IN (?)"
    val inFilterString = s"IN (${List.fill(noOfLifts)("?").mkString(", ")})"
    if (orientQl.contains(inFilterString)) {
      orientQl.replace(inFilterString, orientInFilterString)
    } else {
      orientQl
    }
  }
}