package io.getquill

import java.util.TimeZone
import com.twitter.finagle.client.DefaultPool
import com.twitter.finagle.exp.Mysql
import com.twitter.util.Try
import com.typesafe.config.Config
import com.twitter.util.TimeConversions._

case class FinagleMysqlContextConfig(config: Config) {

  def dateTimezone = TimeZone.getDefault
  def user = config.getString("user")
  def password = Try(config.getString("password")).getOrElse(null)
  def database = config.getString("database")
  def dest = config.getString("dest")
  def lowWatermark = Try(config.getInt("pool.watermark.low")).getOrElse(0)
  def highWatermark = Try(config.getInt("pool.watermark.high")).getOrElse(10)
  def idleTime = Try(config.getInt("pool.idleTime")).getOrElse(5)
  def bufferSize = Try(config.getInt("pool.bufferSize")).getOrElse(0)
  def maxWaiters = Try(config.getInt("pool.maxWaiters")).getOrElse(Int.MaxValue)

  def client =
    Mysql.client
      .withCredentials(user, password)
      .withDatabase(database)
      .configured(DefaultPool.Param(
        low = lowWatermark, high = highWatermark,
        idleTime = idleTime.seconds,
        bufferSize = bufferSize,
        maxWaiters = maxWaiters
      ))
      .newRichClient(dest)
}
