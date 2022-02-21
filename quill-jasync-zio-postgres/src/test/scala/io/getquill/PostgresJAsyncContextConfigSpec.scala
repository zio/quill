package io.getquill

import java.io.File
import com.github.jasync.sql.db.SSLConfiguration
import com.github.jasync.sql.db.SSLConfiguration.Mode
import com.typesafe.config.{ ConfigFactory, ConfigValueFactory }
import io.getquill.context.zio.PostgresJAsyncContextConfig

class PostgresJAsyncContextConfigSpec extends Spec {

  "parses ssl config" in {
    val config = ConfigFactory.empty()
      .withValue("user", ConfigValueFactory.fromAnyRef("user"))
      .withValue("port", ConfigValueFactory.fromAnyRef(5432))
      .withValue("host", ConfigValueFactory.fromAnyRef("host"))
      .withValue("sslmode", ConfigValueFactory.fromAnyRef("require"))
      .withValue("sslrootcert", ConfigValueFactory.fromAnyRef("./file.crt"))
    val context = new PostgresJAsyncContextConfig(config)
    context.connectionPoolConfiguration.getSsl mustEqual new SSLConfiguration(Mode.Require, new File("./file.crt"))
  }
}
