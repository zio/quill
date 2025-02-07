import java.io.{File => JFile}
import com.jsuereth.sbtpgp.PgpKeys.publishSigned

import scala.collection.immutable.ListSet

Global / onChangedBuildSource := ReloadOnSourceChanges

inThisBuild(
  List(
    organization := "io.getquill",
    homepage     := Some(url("https://zio.dev/zio-quill")),
    licenses     := List(("Apache License 2.0", url("http://www.apache.org/licenses/LICENSE-2.0"))),
    developers := List(
      Developer("fwbrasil", "Flavio W. Brasil", "", url("https://github.com/fwbrasil")),
      Developer("deusaquilus", "Alexander Ioffe", "", url("https://github.com/deusaquilus"))
    ),
    scmInfo := Some(
      ScmInfo(url("https://github.com/zio/zio-quill"), "git:git@github.com:zio/zio-quill.git")
    ),
    scalafmtCheck     := true,
    scalafmtSbtCheck  := true,
    scalafmtOnCompile := !insideCI.value
  )
)

val CodegenTag = Tags.Tag("CodegenTag")
(Global / concurrentRestrictions) += Tags.exclusive(CodegenTag)

lazy val baseModules = Seq[sbt.ClasspathDep[sbt.ProjectReference]](
  `quill-engine`,
  `quill-core`,
  `quill-sql`,
  `quill-sql-test`,
  `quill-zio`,
  `quill-util`
)

lazy val docsModules = Seq[sbt.ClasspathDep[sbt.ProjectReference]](
  docs
)

lazy val dbModules = Seq[sbt.ClasspathDep[sbt.ProjectReference]](
  `quill-jdbc`,
  `quill-jdbc-test-h2`,
  `quill-jdbc-test-mysql`,
  `quill-jdbc-test-oracle`,
  `quill-jdbc-test-postgres`,
  `quill-jdbc-test-sqlite`,
  `quill-jdbc-test-sqlserver`,
  `quill-doobie`,
  `quill-jdbc-zio`
)

lazy val codegenModules = Seq[sbt.ClasspathDep[sbt.ProjectReference]](
  `quill-codegen`,
  `quill-codegen-jdbc`
)

lazy val bigdataModules = Seq[sbt.ClasspathDep[sbt.ProjectReference]](
  `quill-cassandra`,
  `quill-cassandra-zio`,
  `quill-cassandra-pekko`,
  `quill-orientdb`,
  `quill-spark`
)

lazy val allModules =
  baseModules ++ dbModules ++ codegenModules ++ bigdataModules ++ docsModules

lazy val scala213Modules =
  baseModules ++ dbModules ++ codegenModules ++ bigdataModules

lazy val scala3Modules = Seq[sbt.ClasspathDep[sbt.ProjectReference]](`quill-engine`, `quill-util`)

def isScala213 = {
  val scalaVersion = sys.props.get("quill.scala.version")
  scalaVersion.map(_.startsWith("2.13")).getOrElse(false)
}

def isScala3 = {
  val scalaVersion = sys.props.get("quill.scala.version")
  scalaVersion.map(_.startsWith("3")).getOrElse(false)
}

def isScala2 = {
  val scalaVersion = sys.props.get("quill.scala.version")
  scalaVersion.map(_.startsWith("2")).getOrElse(false)
}

lazy val filteredModules = {
  val modulesStr = sys.props.get("modules")
  val moduleStrings =
    ListSet(
      modulesStr
        .getOrElse("nocodegen")
        .split(",")
        .map(_.trim): _*
    )

  println(s"SBT =:> Matching Modules ${moduleStrings} from argument value: '${modulesStr}''")

  def matchModules(modulesStr: String) =
    modulesStr match {
      case "base" =>
        println("SBT =:> Compiling Base Modules")
        baseModules
      case "db" =>
        println("SBT =:> Compiling Database Modules")
        dbModules
      case "codegen" =>
        println("SBT =:> Compiling Code Generator Modules")
        codegenModules
      case "nocodegen" =>
        println("Compiling Not-Code Generator Modules")
        baseModules ++ dbModules ++ bigdataModules
      case "bigdata" =>
        println("SBT =:> Compiling Big Data Modules")
        bigdataModules
      case "none" =>
        println("SBT =:> Invoking Aggregate Project")
        Seq[sbt.ClasspathDep[sbt.ProjectReference]]()
      case _ | "all" =>
        println("Compiling Not-Code Generator Modules")
        baseModules ++ dbModules ++ bigdataModules
      // println("SBT =:> Compiling All Modules")
      // allModules
    }

  val selectedModules = {
    val modules =
      moduleStrings
        .map(matchModules)
        .flatMap(seq => ListSet(seq: _*))

    if (isScala213) {
      println("SBT =:> Compiling 2.13 Modules Only")
      modules.filter(scala213Modules.contains(_))
    } else if (isScala3) {
      println("SBT =:> Compiling 3 Modules Only")
      modules.filter(scala3Modules.contains(_))
    } else modules
  }

  println(
    s"=== Selected Modules ===\n${selectedModules.map(_.project.toString).toList.mkString("\n")}\n=== End Selected Modules ==="
  )
  selectedModules
}

lazy val `quill` =
  (project in file("."))
    .settings(commonSettings: _*)
    .settings(
      publishArtifact      := false,
      publish / skip       := true,
      publishLocal / skip  := true,
      publishSigned / skip := true,
      crossScalaVersions   := Nil // https://www.scala-sbt.org/1.x/docs/Cross-Build.html#Cross+building+a+project+statefully
    )
    .aggregate(filteredModules.map(_.project).toSeq: _*)

lazy val `quill-util` =
  (project in file("quill-util"))
    .settings(commonSettings: _*)
    .settings(
      Test / fork := true,
      libraryDependencies ++= Seq(
        ("org.scalameta" %% "scalafmt-core" % "3.8.6")
          .excludeAll(
            ({
              if (isScala3)
                Seq(
                  ExclusionRule(organization = "org.scala-lang.modules", name = "scala-collection-compat_2.13"),
                  ExclusionRule(organization = "org.scala-lang.modules", name = "scala-parallel-collections_2.13"),
                  ExclusionRule(organization = "com.lihaoyi", name = "sourcecode_2.13"),
                  ExclusionRule(organization = "com.lihaoyi", name = "fansi_2.13")
                )
              else
                Seq.empty
            }): _*
          )
          .cross(CrossVersion.for3Use2_13)
      ) ++ {
        if (isScala3)
          Seq(
            // transitive dependencies of scalafmt-core
            // update when updating scalafmt-core
            "org.scala-lang.modules" %% "scala-collection-compat"    % scalaCollectionCompatVersion,
            "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4",
            "com.lihaoyi"            %% "sourcecode"                 % "0.3.0",
            "com.lihaoyi"            %% "fansi"                      % "0.3.0"
          )
        else Seq.empty
      }
    )
    .enablePlugins(MimaPlugin)

lazy val `quill-engine` =
  project
    .settings(commonSettings: _*)
    .settings(
      libraryDependencies ++= Seq(
        "com.typesafe"                  % "config"        % "1.4.3",
        "com.typesafe.scala-logging"   %% "scala-logging" % "3.9.5",
        ("com.github.takayahilton"     %% "sql-formatter" % "1.2.1").cross(CrossVersion.for3Use2_13),
        "io.suzaku"                    %% "boopickle"     % "1.5.0",
        "com.lihaoyi"                  %% "pprint"        % "0.9.0",
        "com.github.ben-manes.caffeine" % "caffeine"      % "3.2.0"
      ),
      coverageExcludedPackages := "<empty>;.*AstPrinter;.*Using;io.getquill.Model;io.getquill.ScalarTag;io.getquill.QuotationTag"
    )
    .enablePlugins(MimaPlugin)

lazy val `quill-core` =
  project
    .settings(commonSettings: _*)
    .settings(
      libraryDependencies ++= Seq(
        "com.typesafe"                % "config"        % "1.4.3",
        "dev.zio"                    %% "zio-logging"   % "2.4.0",
        "dev.zio"                    %% "zio"           % Version.zio,
        "dev.zio"                    %% "zio-streams"   % Version.zio,
        "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5"
      ),
      Test / fork := true
    )
    .dependsOn(`quill-engine` % "compile->compile")
    .enablePlugins(MimaPlugin)

lazy val `quill-sql` =
  project
    .settings(commonSettings: _*)
    .dependsOn(
      `quill-engine` % "compile->compile",
      `quill-core`   % "compile->compile"
    )
    .enablePlugins(MimaPlugin)

lazy val `quill-sql-test` =
  project
    .settings(commonSettings: _*)
    .settings(noPublishSettings: _*)
    .dependsOn(
      `quill-sql`,
      `quill-core`     % "test->test",
      `quill-test-kit` % "test->test"
    )
    .enablePlugins(MimaPlugin)

lazy val `quill-codegen` =
  (project in file("quill-codegen"))
    .settings(commonSettings: _*)
    .dependsOn(`quill-core` % "compile->compile;test->test")

lazy val `quill-codegen-jdbc` =
  (project in file("quill-codegen-jdbc"))
    .settings(commonSettings: _*)
    .settings(jdbcTestingLibraries: _*)
    .settings(
      Test / fork := true,
      libraryDependencies ++= Seq(
        "org.scala-lang" % "scala-compiler" % scalaVersion.value % Test
      )
    )
    .dependsOn(`quill-codegen` % "compile->compile;test->test")
    .dependsOn(`quill-jdbc` % "compile->compile")

//lazy val `quill-codegen-tests` =
//  (project in file("quill-codegen-tests"))
//    .settings(commonSettings: _*)
//    .settings(
//      publish / skip                         := true,
//      libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value % Test,
//      Test / fork                            := true,
//      (Test / sourceGenerators) += Def.task {
//        def recursiveList(file: JFile): List[JFile] =
//          if (file.isDirectory)
//            Option(file.listFiles()).map(_.flatMap(child => recursiveList(child)).toList).toList.flatten
//          else
//            List(file)
//        val r          = (Compile / runner).value
//        val s          = streams.value.log
//        val sourcePath = sourceManaged.value
//        val classPath  = (`quill-codegen-jdbc` / Test / fullClasspath).value.map(_.data)
//
//        // We could put the code generated products directly in the `sourcePath` directory but for some reason
//        // intellij doesn't like it unless there's a `main` directory inside.
//        val fileDir = new File(sourcePath, "main").getAbsoluteFile
//        val dbs     = Seq("testH2DB", "testMysqlDB", "testPostgresDB", "testSqliteDB", "testSqlServerDB", "testOracleDB")
//        println(s"Running code generation for DBs: ${dbs.mkString(", ")}")
//        r.run(
//          "io.getquill.codegen.integration.CodegenTestCaseRunner",
//          classPath,
//          fileDir.getAbsolutePath +: dbs,
//          s
//        )
//        recursiveList(fileDir)
//      }.tag(CodegenTag)
//    )
//    .dependsOn(`quill-codegen-jdbc` % "compile->test")

val excludeTests =
  sys.props.getOrElse("excludeTests", "false") match {
    case "false" => ExcludeTests.Include
    case "true"  => ExcludeTests.Exclude
    case regex   => ExcludeTests.KeepSome(regex)
  }

val debugMacro =
  sys.props.getOrElse("debugMacro", "false").toBoolean

lazy val `quill-jdbc` =
  (project in file("quill-jdbc"))
    .settings(commonSettings: _*)
    .settings(jdbcTestingSettings: _*)
    .dependsOn(
      `quill-sql`      % "compile->compile",
      `quill-core`     % "test->test",
      `quill-test-kit` % "test->test"
    )
    .enablePlugins(MimaPlugin)

lazy val `quill-jdbc-test-h2` =
  (project in file("quill-jdbc-test-h2"))
    .settings(commonSettings: _*)
    .settings(jdbcTestingSettings: _*)
    .settings(noPublishSettings: _*)
    .dependsOn(
      `quill-jdbc`     % "compile->compile;test->test",
      `quill-test-kit` % "test->test"
    )
    .enablePlugins(MimaPlugin)

lazy val `quill-jdbc-test-mysql` =
  (project in file("quill-jdbc-test-mysql"))
    .settings(commonSettings: _*)
    .settings(jdbcTestingSettings: _*)
    .settings(noPublishSettings: _*)
    .dependsOn(
      `quill-jdbc`     % "compile->compile;test->test",
      `quill-test-kit` % "test->test"
    )
    .enablePlugins(MimaPlugin)

lazy val `quill-jdbc-test-oracle` =
  (project in file("quill-jdbc-test-oracle"))
    .settings(commonSettings: _*)
    .settings(jdbcTestingSettings: _*)
    .settings(noPublishSettings: _*)
    .dependsOn(
      `quill-jdbc`     % "compile->compile;test->test",
      `quill-test-kit` % "test->test"
    )
    .enablePlugins(MimaPlugin)

lazy val `quill-jdbc-test-postgres` =
  (project in file("quill-jdbc-test-postgres"))
    .settings(commonSettings: _*)
    .settings(jdbcTestingSettings: _*)
    .settings(noPublishSettings: _*)
    .dependsOn(
      `quill-jdbc`     % "compile->compile;test->test",
      `quill-test-kit` % "test->test"
    )
    .enablePlugins(MimaPlugin)

lazy val `quill-jdbc-test-sqlite` =
  (project in file("quill-jdbc-test-sqlite"))
    .settings(commonSettings: _*)
    .settings(jdbcTestingSettings: _*)
    .settings(noPublishSettings: _*)
    .dependsOn(
      `quill-jdbc`     % "compile->compile;test->test",
      `quill-test-kit` % "test->test"
    )
    .enablePlugins(MimaPlugin)

lazy val `quill-jdbc-test-sqlserver` =
  (project in file("quill-jdbc-test-sqlserver"))
    .settings(commonSettings: _*)
    .settings(jdbcTestingSettings: _*)
    .settings(noPublishSettings: _*)
    .dependsOn(
      `quill-jdbc`     % "compile->compile;test->test",
      `quill-test-kit` % "test->test"
    )
    .enablePlugins(MimaPlugin)

ThisBuild / libraryDependencySchemes += "org.typelevel" %% "cats-effect" % "always"
lazy val `quill-doobie` =
  (project in file("quill-doobie"))
    .settings(commonSettings: _*)
    .settings(jdbcTestingSettings: _*)
    .settings(
      libraryDependencies ++= Seq(
        "org.tpolecat" %% "doobie-core"     % "1.0.0-RC5",
        "org.tpolecat" %% "doobie-postgres" % "1.0.0-RC5" % Test
      )
    )
    .dependsOn(
      `quill-jdbc`     % "compile->compile",
      `quill-sql-test` % "test->test"
    )
    .enablePlugins(MimaPlugin)

lazy val `quill-zio` =
  (project in file("quill-zio"))
    .settings(commonSettings: _*)
    .settings(
      Test / fork := true,
      libraryDependencies ++= Seq(
        "dev.zio" %% "zio"         % Version.zio,
        "dev.zio" %% "zio-streams" % Version.zio
      )
    )
    .dependsOn(`quill-core` % "compile->compile;test->test")
    .enablePlugins(MimaPlugin)

lazy val `quill-jdbc-zio` =
  (project in file("quill-jdbc-zio"))
    .settings(commonSettings: _*)
    .settings(jdbcTestingSettings: _*)
    .settings(
      libraryDependencies ++= Seq(
        // Needed for PGObject in JsonExtensions but not necessary if user is not using postgres
        "org.postgresql" % "postgresql" % "42.7.5" % "provided",
        "dev.zio"       %% "zio-json"   % "0.7.4"
      ),
      Test / testGrouping := {
        (Test / definedTests).value map { test =>
          if (test.name endsWith "IntegrationSpec")
            Tests.Group(
              name = test.name,
              tests = Seq(test),
              runPolicy = Tests.SubProcess(
                ForkOptions().withRunJVMOptions(Vector("-Xmx200m"))
              )
            )
          else
            Tests.Group(
              name = test.name,
              tests = Seq(test),
              runPolicy = Tests.SubProcess(ForkOptions())
            ) // .withRunJVMOptions(Vector("-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=5005"))
        }
      }
    )
    .dependsOn(`quill-zio` % "compile->compile;test->test")
    .dependsOn(`quill-sql` % "compile->compile")
    .dependsOn(`quill-jdbc` % "compile->compile;test->test")
    .enablePlugins(MimaPlugin)

lazy val `quill-spark` =
  (project in file("quill-spark"))
    .settings(commonNoLogSettings: _*)
    .settings(
      Test / fork := true,
      libraryDependencies ++= Seq("org.apache.spark" %% "spark-sql" % "3.4.0"),
      excludeDependencies ++= Seq("ch.qos.logback" % "logback-classic")
    )
    .dependsOn(
      `quill-sql`      % "compile->compile",
      `quill-sql-test` % "test->test",
      `quill-core`     % "test->test"
    )
    .enablePlugins(MimaPlugin)

lazy val `quill-cassandra` =
  (project in file("quill-cassandra"))
    .settings(commonSettings: _*)
    .settings(
      Test / fork := true,
      libraryDependencies ++= Seq(
        "com.datastax.oss"        % "java-driver-core"   % "4.17.0",
        "org.scala-lang.modules" %% "scala-java8-compat" % "1.0.2"
      )
    )
    .dependsOn(`quill-core` % "compile->compile;test->test")
    .enablePlugins(MimaPlugin)

lazy val `quill-cassandra-zio` =
  (project in file("quill-cassandra-zio"))
    .settings(commonSettings: _*)
    .settings(
      Test / fork := true,
      libraryDependencies ++= Seq(
        "dev.zio" %% "zio"         % Version.zio,
        "dev.zio" %% "zio-streams" % Version.zio
      )
    )
    .dependsOn(`quill-cassandra` % "compile->compile;test->test")
    .dependsOn(`quill-zio` % "compile->compile;test->test")
    .enablePlugins(MimaPlugin)

lazy val `quill-cassandra-pekko` =
  (project in file("quill-cassandra-pekko"))
    .settings(commonSettings: _*)
    .settings(
      Test / fork := true,
      libraryDependencies ++= Seq(
        "org.apache.pekko" %% "pekko-connectors-cassandra" % "1.1.0",
        "org.apache.pekko" %% "pekko-testkit"              % "1.0.2" % Test
      )
    )
    .dependsOn(`quill-cassandra` % "compile->compile;test->test")
    .enablePlugins(MimaPlugin)

lazy val `quill-orientdb` =
  (project in file("quill-orientdb"))
    .settings(commonSettings: _*)
    .settings(
      Test / fork := true,
      libraryDependencies ++= Seq(
        "com.orientechnologies" % "orientdb-graphdb" % "3.2.37"
      )
    )
    .dependsOn(
      `quill-sql`  % "compile->compile",
      `quill-core` % "test->test"
    )
    .enablePlugins(MimaPlugin)

lazy val `quill-test-kit` =
  (project in file("quill-test-kit"))
    .settings(commonSettings: _*)
    .settings(noPublishSettings: _*)
    .dependsOn(
      `quill-sql`,
      `quill-core` % "test->test"
    )
    .enablePlugins(MimaPlugin)

lazy val jdbcTestingLibraries = Seq(
  libraryDependencies ++= Seq(
    "com.zaxxer"              % "HikariCP"                % "6.2.1" exclude ("org.slf4j", "*"),
    "com.mysql"               % "mysql-connector-j"       % "9.2.0"       % Test,
    "com.h2database"          % "h2"                      % "2.3.232"     % Test,
    "org.postgresql"          % "postgresql"              % "42.7.5"      % Test,
    "org.xerial"              % "sqlite-jdbc"             % "3.48.0.0"    % Test,
    "com.microsoft.sqlserver" % "mssql-jdbc"              % "7.4.1.jre11" % Test,
    "com.oracle.ojdbc"        % "ojdbc8"                  % "19.3.0.0"    % Test,
    "org.mockito"            %% "mockito-scala-scalatest" % "1.17.14"     % Test
  )
)

lazy val excludeFilterSettings = Seq(
  unmanagedSources / excludeFilter := {
    lazy val paths =
      (Test / unmanagedSourceDirectories).value
        .map(dir => dir.getCanonicalPath)

    excludeTests match {
      case ExcludeTests.Include =>
        excludePaths(List.empty)
      case _ =>
        excludePaths(paths)
    }
  }
)

lazy val jdbcTestingSettings = excludeFilterSettings ++ jdbcTestingLibraries ++ Seq(
  Test / fork := true
)

def excludePaths(paths: Seq[String]) = {
  def isBasePathExcluded(path: String) =
    paths.exists { srcDir =>
      (path contains srcDir)
    }
  def isKeptOverride(path: String): Boolean =
    excludeTests match {
      case ExcludeTests.KeepSome(regex) =>
        def keepFilter(path: String) = {
          val keep =
            path.matches(regex) ||
              path.contains("io/getquill/context/sql/base") ||
              path.contains("io/getquill/context/sql/ProductSpec") ||
              path.contains("TestContext") ||
              path.contains("package.scala") ||
              path.contains("oracle.scala") ||
              path.contains("io/getquill/UpperCaseNonDefault") ||
              path.contains("io/getquill/base") ||
              path.contains("io/getquill/TestEntities") ||
              path.contains("io/getquill/context/sql/TestEncoders") ||
              path.contains("io/getquill/context/sql/TestDecoders") ||
              path.contains("io/getquill/context/sql/encoding/ArrayEncodingBaseSpec") ||
              path.contains("EncodingSpec")
          if (keep) println(s"KEEPING: ${path}")
          keep
        }
        keepFilter(path)
      case _ => false
    }
  new SimpleFileFilter(file => {
    val exclude = isBasePathExcluded(file.getCanonicalPath) && !isKeptOverride(file.getCanonicalPath)
    if (exclude) println(s"Excluding: ${file.getCanonicalPath}")
    exclude
  })
}

val scala_v_12 = "2.12.20"
val scala_v_13 = "2.13.16"
val scala_v_30 = "3.3.5"

val scalaCollectionCompatVersion = "2.13.0"

lazy val loggingSettings = Seq(
  libraryDependencies ++= Seq(
    "ch.qos.logback" % "logback-classic" % "1.5.16" % Test
  )
)

lazy val basicSettings = excludeFilterSettings ++ Seq(
  Test / testOptions += Tests.Argument("-oI"),
  scalaVersion       := scala_v_13,
  crossScalaVersions := Seq(scala_v_12, scala_v_13, scala_v_30),
  libraryDependencies ++= Seq(
    "org.scalatest"           %% "scalatest"               % "3.2.19" % Test,
    "org.scala-lang.modules"  %% "scala-collection-compat" % scalaCollectionCompatVersion,
    "com.google.code.findbugs" % "jsr305"                  % "3.0.2"  % Provided // just to avoid warnings during compilation
  ) ++ {
    if (debugMacro && isScala2)
      Seq(
        "org.scala-lang" % "scala-library"  % scalaVersion.value,
        "org.scala-lang" % "scala-compiler" % scalaVersion.value,
        "org.scala-lang" % "scala-reflect"  % scalaVersion.value
      )
    else Seq.empty
  } ++ {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, _)) => Seq(compilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"))
      case _            => Seq.empty
    }
  },
  Test / unmanagedClasspath ++= Seq(
    baseDirectory.value / "src" / "test" / "resources"
  ),
  javacOptions := Seq("-source", "11", "-target", "11"),
  scalacOptions ++= Seq(
    "-release:11",
    "-encoding",
    "UTF-8",
    "-feature",
    "-deprecation",
    "-unchecked",
    "-Ywarn-dead-code",
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard",
    "-Ypatmat-exhaust-depth",
    "40"
  ),
  scalacOptions ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 13)) =>
        Seq("-Ypatmat-exhaust-depth", "40")
      case Some((2, 12)) =>
        Seq(
          // "-Xfatal-warnings",
          "-Xlint:-unused,_",
          "-Xfuture",
          "-deprecation",
          "-Yno-adapted-args",
          "-Ywarn-unused:imports",
          "-Ycache-macro-class-loader:last-modified"
        )
      case _ => Seq.empty
    }
  },
  Global / concurrentRestrictions += Tags.limit(Tags.Test, 1),
  scoverage.ScoverageKeys.coverageMinimumStmtTotal := 96,
  scoverage.ScoverageKeys.coverageFailOnMinimum    := false
)

lazy val commonNoLogSettings = basicSettings
lazy val commonSettings      = basicSettings ++ loggingSettings

lazy val noPublishSettings = Seq(
  publish         := {},
  publishLocal    := {},
  publishM2       := {},
  publishArtifact := false
)

lazy val docs = project
  .in(file("zio-quill-docs"))
  .enablePlugins(WebsitePlugin)
  .settings(coverageEnabled := false)
  .settings(
    moduleName := "zio-quill-docs",
    scalacOptions -= "-Yno-imports",
    scalacOptions -= "-Xfatal-warnings",
    scalacOptions += "-Xlog-implicits",
    libraryDependencies ++= Seq("dev.zio" %% "zio" % Version.zio) ++ {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, _)) => Seq(compilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"))
        case _            => Seq.empty
      }
    },
    projectName    := "ZIO Quill",
    mainModuleName := (`quill-core` / moduleName).value,
    // With Scala 2.12, these projects doc isn't compiling.
    ScalaUnidoc / unidoc / unidocProjectFilter := inAnyProject -- inProjects(
      `quill-engine`,
      `quill-core`,
      `quill-orientdb`,
      `quill-doobie`
    ),
    projectStage                          := ProjectStage.ProductionReady,
    checkArtifactBuildProcessWorkflowStep := None,
    docsPublishBranch                     := "master",
    readmeBanner :=
      """|<p align="center">
         |  <img src="https://raw.githubusercontent.com/getquill/quill/master/quill.png">
         |</p>
         |""".stripMargin,
    readmeAcknowledgement :=
      """|The project was created having Philip Wadler's talk ["A practical theory of language-integrated query"](https://www.infoq.com/presentations/theory-language-integrated-query) as its initial inspiration. The development was heavily influenced by the following papers:
         |
         |* [A Practical Theory of Language-Integrated Query](https://homepages.inf.ed.ac.uk/slindley/papers/practical-theory-of-linq.pdf)
         |* [Everything old is new again: Quoted Domain Specific Languages](https://homepages.inf.ed.ac.uk/wadler/papers/qdsl/qdsl.pdf)
         |* [The Flatter, the Better](https://db.inf.uni-tuebingen.de/staticfiles/publications/the-flatter-the-better.pdf)""".stripMargin,
    readmeMaintainers :=
      """|- @deusaquilus (lead maintainer)
         |- @fwbrasil (creator)
         |- @jilen
         |- @juliano
         |- @mentegy
         |- @mdedetrich
         |
         |### Former maintainers:
         |
         |- @gustavoamigo
         |- @godenji
         |- @lvicentesanchez
         |- @mxl
         |
         |You can notify all current maintainers using the handle `@getquill/maintainers`.""".stripMargin
  )
