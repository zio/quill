package io.getquill.codegen.integration

import java.nio.file.Path

import com.typesafe.config.ConfigValueFactory
import io.getquill.codegen.integration.SchemaNames.{simpleLiteral, simpleSnake, twoSchema}
import io.getquill.codegen.jdbc.DatabaseTypes._
import io.getquill.codegen.jdbc.gen.JdbcCodeGeneratorComponents
import io.getquill.codegen.jdbc.model.JdbcTypes.JdbcQuerySchemaNaming
import io.getquill.codegen.jdbc.{ComposeableTraitsJdbcCodegen, SimpleJdbcCodegen}
import io.getquill.codegen.model.Stereotyper.Namespacer
import io.getquill.codegen.model._
import io.getquill.codegen.util.ConfigPrefix.{TestOracleDB, TestSqlServerDB, TestSqliteDB}
import io.getquill.codegen.util.StringUtil._
import io.getquill.codegen.util.{ConfigPrefix, SchemaConfig, SchemaMaker, SchemaMakerCoordinates}
import io.getquill.{JdbcContextConfig, NamingStrategy, SnakeCase}

import scala.concurrent.Future

class TestInfo(val naming: NamingStrategy, val schemaConfig: SchemaConfig)

sealed trait CodegenTestCases {
  val naming: NamingStrategy
  val schemaConfig: SchemaConfig
  def testName: String = {
    import scala.reflect.runtime.{universe => u}
    val m   = u.runtimeMirror(this.getClass.getClassLoader)
    val sym = m.reflect(this).symbol
    sym.name.decodedName.toString + "-lib"
  }

  def schemaMakerCoordinates(dbPrefix: ConfigPrefix) = SchemaMakerCoordinates(dbPrefix, naming, schemaConfig)

  def generateWithSchema(dbPrefix: ConfigPrefix, basePath: String): Future[Seq[Path]] =
    SchemaMaker.withContext(schemaMakerCoordinates(dbPrefix))(
      generate(dbPrefix, basePath)
    )

  protected def generate(dbPrefix: ConfigPrefix, basePath: String): Future[Seq[Path]]
  protected def pathList(implicit dbPrefix: ConfigPrefix) =
    List("io", "getquill", "codegen", "generated", dbPrefix.packagePath)
  protected def path(implicit dbPrefix: ConfigPrefix, basePath: String) =
    (basePath +: pathList :+ testName).mkString("/")
  protected def `package`(implicit dbPrefix: ConfigPrefix) = (pathList :+ s"`${testName}`").mkString(".")
}

object CodegenTestCases {

  trait `1-simple-snake`              extends CodegenTestCases
  trait `2-simple-literal`            extends CodegenTestCases
  trait `1-comp-sanity`               extends CodegenTestCases
  trait `2-comp-stereo-single`        extends CodegenTestCases
  trait `3-comp-stereo-oneschema`     extends CodegenTestCases
  trait `4-comp-stereo-twoschema`     extends CodegenTestCases
  trait `5-comp-non-stereo-allschema` extends CodegenTestCases

  case object `1-simple-snake` extends TestInfo(SnakeCase, simpleSnake) with `1-simple-snake` {
    override def generate(dbPrefix: ConfigPrefix, basePath: String) = {
      implicit val prefix = dbPrefix
      implicit val base   = basePath
      val snakeCaseGen = new SimpleJdbcCodegen(dbPrefix.loadConfig, `package`) with DatabaseBasedNumericTyping {
        override def defaultNamespace: String = "public"
        override def nameParser               = SnakeCaseNames
        override def filter(tc: RawSchema[JdbcTableMeta, JdbcColumnMeta]): Boolean =
          schemaFilter(databaseType, tc) && super.filter(tc)
        override def namespacer: Namespacer[JdbcTableMeta] = ts => {
          if (super.namespacer(ts) startsWith "codegenTest") "public" else super.namespacer(ts)
        }
      }
      snakeCaseGen.writeAllFiles(path)
    }
  }

  case object `2-simple-literal` extends TestInfo(SnakeCase, simpleLiteral) with `2-simple-literal` {
    override def generate(dbPrefix: ConfigPrefix, basePath: String) = {
      implicit val prefix = dbPrefix
      implicit val base   = basePath
      val literalGen = new SimpleJdbcCodegen(dbPrefix.loadConfig, `package`) with DatabaseBasedNumericTyping {
        override def defaultNamespace: String = "public"
        override def nameParser               = LiteralNames // Should be default
        override def filter(tc: RawSchema[JdbcTableMeta, JdbcColumnMeta]): Boolean =
          schemaFilter(databaseType, tc) && super.filter(tc)
        override def namespacer: Namespacer[JdbcTableMeta] = ts => {
          if (super.namespacer(ts) startsWith "codegenTest") "public" else super.namespacer(ts)
        }
      }
      literalGen.writeAllFiles(path)
    }
  }

  case object `1-comp-sanity` extends TestInfo(SnakeCase, simpleSnake) with `1-comp-sanity` {
    override def generate(dbPrefix: ConfigPrefix, basePath: String) = {
      implicit val prefix = dbPrefix
      implicit val base   = basePath
      val gen = new ComposeableTraitsJdbcCodegen(dbPrefix.loadConfig, `package`, false)
        with DatabaseBasedNumericTyping {
        override def defaultNamespace: String = "public"
        override def nameParser: NameParser   = SnakeCaseNames
        override def filter(tc: RawSchema[JdbcTableMeta, JdbcColumnMeta]): Boolean =
          schemaFilter(databaseType, tc) && super.filter(tc)
        override def namespacer: Namespacer[JdbcTableMeta] = ts => {
          if (super.namespacer(ts) startsWith "codegenTest") "public" else super.namespacer(ts)
        }
      }
      gen.writeAllFiles(path)
    }
  }

  case object `2-comp-stereo-single` extends TestInfo(SnakeCase, simpleSnake) with `2-comp-stereo-single` {
    override def generate(dbPrefix: ConfigPrefix, basePath: String) = {
      implicit val prefix = dbPrefix
      implicit val base   = basePath
      val gen = new ComposeableTraitsJdbcCodegen(dbPrefix.loadConfig, `package`, false)
        with DatabaseBasedNumericTyping {
        override def defaultNamespace: String = "public"
        override def nameParser: NameParser =
          CustomNames(col => col.columnName.toLowerCase.replace("_name", ""))
        override def filter(tc: RawSchema[JdbcTableMeta, JdbcColumnMeta]): Boolean =
          schemaFilter(databaseType, tc) && super.filter(tc)
        override def namespacer: Namespacer[JdbcTableMeta] = ts => {
          if (super.namespacer(ts) startsWith "codegenTest") "public" else super.namespacer(ts)
        }
      }
      gen.writeAllFiles(path)
    }
  }

  case object `3-comp-stereo-oneschema` extends TestInfo(SnakeCase, twoSchema) with `3-comp-stereo-oneschema` {
    override def generate(dbPrefix: ConfigPrefix, basePath: String) = {
      implicit val prefix = dbPrefix
      implicit val base   = basePath
      val gen = new ComposeableTraitsJdbcCodegen(
        databaseConnections,
        `package`,
        nestedTrait = true
      ) with DatabaseBasedNumericTyping {
        override def defaultNamespace: String = "public"
        override def nameParser: NameParser   = CustomNames()
        override def filter(tc: RawSchema[JdbcTableMeta, JdbcColumnMeta]): Boolean =
          schemaFilter(databaseType, tc) && super.filter(tc)
        override def namespacer: Namespacer[JdbcTableMeta] = ts => "public"
        override def querySchemaNaming: JdbcQuerySchemaNaming = ts => {
          val schema = super.namespacer(ts)
          (if (schema startsWith "codegenTest") "public" else schema) + ts.tableName.toLowerCase.capitalize
        }
      }

      gen.writeAllFiles(path)
    }
  }

  case object `4-comp-stereo-twoschema` extends TestInfo(SnakeCase, twoSchema) with `4-comp-stereo-twoschema` {
    override def generate(dbPrefix: ConfigPrefix, basePath: String) = {
      implicit val prefix = dbPrefix
      implicit val base   = basePath
      val gen = new ComposeableTraitsJdbcCodegen(databaseConnections, `package`, false)
        with DatabaseBasedNumericTyping {
        override def defaultNamespace: String = "public"
        override def nameParser: NameParser   = CustomNames()
        override def filter(tc: RawSchema[JdbcTableMeta, JdbcColumnMeta]): Boolean =
          schemaFilter(databaseType, tc) && super.filter(tc)
        override def querySchemaNaming: JdbcQuerySchemaNaming = ts => {
          val schema = super.namespacer(ts)
          (if (schema == "codegenTest") "public" else schema) + ts.tableName.toLowerCase.capitalize
        }
        override def namespacer: Namespacer[JdbcTableMeta] = ts => {
          val schema = super.namespacer(ts)
          if (schema == "alpha" || schema == "bravo") "common"
          else {
            databaseType match {
              case MySql | Oracle | SqlServer if (schema startsWith "codegenTest") => "public"
              case _                                                               => schema.toLowerCase
            }
          }
        }
      }

      gen.writeAllFiles(path)
    }
  }

  case object `5-comp-non-stereo-allschema` extends TestInfo(SnakeCase, twoSchema) with `5-comp-non-stereo-allschema` {
    override def generate(dbPrefix: ConfigPrefix, basePath: String) = {
      implicit val prefix = dbPrefix
      implicit val base   = basePath
      val gen = new ComposeableTraitsJdbcCodegen(databaseConnections, `package`, nestedTrait = true)
        with DatabaseBasedNumericTyping {
        override def defaultNamespace: String = "public"
        override def filter(tc: RawSchema[JdbcTableMeta, JdbcColumnMeta]): Boolean =
          schemaFilter(databaseType, tc) && super.filter(tc)
        override def nameParser: NameParser = CustomNames()
        override def namespacer: Namespacer[JdbcTableMeta] = ts => {
          val schema = super.namespacer(ts)
          databaseType match {
            case MySql | Oracle | SqlServer if (schema startsWith "codegenTest") => "public"
            case _                                                               => schema.toLowerCase
          }
        }
      }

      gen.writeAllFiles(path)
    }
  }

  /**
   * Oracle does not really have any 32/64 bit Integer types so basically any
   * Integer column that you create will be a BigInteger. Specifically for
   * Oracle, use a behavior that will attempt to use a primitive datatype based
   * on on the scale of the column.
   */
  trait DatabaseBasedNumericTyping extends JdbcCodeGeneratorComponents {
    def databaseType: DatabaseType
    override def numericPreference: NumericPreference = databaseType match {
      case Oracle => PreferPrimitivesWhenPossible
      case _      => UseDefaults
    }
  }

  private[integration] def databaseConnections(implicit prefix: ConfigPrefix) = prefix match {
    case TestOracleDB =>
      Seq(
        JdbcContextConfig(prefix.loadConfig).dataSource,
        JdbcContextConfig(
          prefix.loadConfig.withValue("dataSource.user", ConfigValueFactory.fromAnyRef("alpha"))
        ).dataSource,
        JdbcContextConfig(
          prefix.loadConfig.withValue("dataSource.user", ConfigValueFactory.fromAnyRef("bravo"))
        ).dataSource
      ).map(ds => () => ds.getConnection)
    case TestSqlServerDB =>
      Seq(
        JdbcContextConfig(prefix.loadConfig).dataSource,
        JdbcContextConfig(
          prefix.loadConfig.withValue("dataSource.databaseName", ConfigValueFactory.fromAnyRef("alpha"))
        ).dataSource,
        JdbcContextConfig(
          prefix.loadConfig.withValue("dataSource.databaseName", ConfigValueFactory.fromAnyRef("bravo"))
        ).dataSource
      ).map(ds => () => ds.getConnection)
    case _ => Seq(() => JdbcContextConfig(prefix.loadConfig).dataSource.getConnection)
  }

  private[integration] def schemaFilter(databaseType: DatabaseType, tc: RawSchema[JdbcTableMeta, JdbcColumnMeta]) =
    databaseType match {
      case MySql =>
        tc.table.tableCat.exists(_.inSetNocase("codegen_test", "alpha", "bravo"))
      // We don't care about schema filtration in oracle since for oracle one only specified schemas are processed
      // see `databaseConnections` for more detail. In SQLite, separate schemas don't exist at all so we have to include everything.
      case Oracle | Sqlite =>
        true
      case H2 =>
        tc.table.tableCat.exists(_.toLowerCase startsWith "codegen_test")
      case Postgres =>
        tc.table.tableSchema.existsInSetNocase("public", "alpha", "bravo")
      case SqlServer =>
        tc.table.tableCat.existsInSetNocase("codegen_test", "alpha", "bravo")
    }

  def apply(dbPrefix: ConfigPrefix): List[CodegenTestCases] =
    dbPrefix match {
      // SQLite does not support user-defined schemas. It has the ability to use multiple files
      // but does not show what table belongs to what file in any JDBC call. This makes multi-schema
      // stereotyping untenable so the respective tests are not included.
      case TestSqliteDB =>
        List(
          `1-simple-snake`,
          `2-simple-literal`,
          `1-comp-sanity`,
          `2-comp-stereo-single`
        )
      case _ =>
        List(
          `1-simple-snake`,
          `2-simple-literal`,
          `1-comp-sanity`,
          `2-comp-stereo-single`,
          `3-comp-stereo-oneschema`,
          `4-comp-stereo-twoschema`,
          `5-comp-non-stereo-allschema`
        )
    }

}
