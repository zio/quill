package io.getquill

import java.io.Closeable

import com.typesafe.config.Config
import io.getquill.context.jdbc.SqlServerJdbcContextBase
import io.getquill.context.monix.MonixJdbcContext
import io.getquill.context.monix.MonixJdbcContext.Runner
import io.getquill.util.LoadConfig
import javax.sql.DataSource

class SqlServerMonixJdbcContext[N <: NamingStrategy](
  val naming:     N,
  val dataSource: DataSource with Closeable,
  runner:         Runner
) extends MonixJdbcContext[SQLServerDialect, N](dataSource, runner)
  with SqlServerJdbcContextBase[N] {

  def this(naming: N, config: JdbcContextConfig, runner: Runner) = this(naming, config.dataSource, runner)
  def this(naming: N, config: Config, runner: Runner) = this(naming, JdbcContextConfig(config), runner)
  def this(naming: N, configPrefix: String, runner: Runner) = this(naming, LoadConfig(configPrefix), runner)
  def this(naming: N, configPrefix: String) = this(naming, LoadConfig(configPrefix), Runner.default)
}