import ReleaseTransformations._
import com.typesafe.sbt.SbtScalariform.ScalariformKeys
import scalariform.formatter.preferences._
import sbtrelease.ReleasePlugin
import scala.sys.process.Process
import sbtcrossproject.crossProject

enablePlugins(TutPlugin)

lazy val sparkIncludeProp = Option(System.getProperty("spark.include"))

lazy val modules = Seq[sbt.ClasspathDep[sbt.ProjectReference]](
  `quill-core-jvm`, `quill-core-js`, `quill-sql-jvm`, `quill-sql-js`,
  `quill-jdbc`, `quill-finagle-mysql`, `quill-finagle-postgres`, `quill-async`,
  `quill-async-mysql`, `quill-async-postgres`, `quill-cassandra`, `quill-orientdb`
) ++ 
  Seq[sbt.ClasspathDep[sbt.ProjectReference]](`quill-spark`)
    .filter(_ => sparkIncludeProp.contains("true"))

lazy val `quill` =
  (project in file("."))
    .settings(commonSettings)
    .settings(`tut-settings`:_*)
    .aggregate(modules.map(_.project): _*)
    .dependsOn(modules: _*)

lazy val superPure = new sbtcrossproject.CrossType {
  def projectDir(crossBase: File, projectType: String): File =
    projectType match {
      case "jvm" => crossBase
      case "js"  => crossBase / s".$projectType"
    }

  def sharedSrcDir(projectBase: File, conf: String): Option[File] =
    Some(projectBase.getParentFile / "src" / conf / "scala")

  override def projectDir(crossBase: File, projectType: sbtcrossproject.Platform): File =
    projectType match {
      case JVMPlatform => crossBase
      case JSPlatform  => crossBase / ".js"
    }
}

lazy val `quill-core` =
  crossProject(JVMPlatform, JSPlatform).crossType(superPure)
    .settings(commonSettings: _*)
    .settings(mimaSettings: _*)
    .settings(libraryDependencies ++= Seq(
      "com.typesafe"               %  "config"        % "1.3.3",
      "com.typesafe.scala-logging" %% "scala-logging" % "3.9.0",
      "org.scala-lang"             %  "scala-reflect" % scalaVersion.value
    ))
    .jsSettings(
      libraryDependencies += "org.scala-js" %%% "scalajs-java-time" % "0.2.5",
      coverageExcludedPackages := ".*"
    )

lazy val `quill-core-jvm` = `quill-core`.jvm
lazy val `quill-core-js` = `quill-core`.js

lazy val `quill-sql` =
  crossProject(JVMPlatform, JSPlatform).crossType(superPure)
    .settings(commonSettings: _*)
    .settings(mimaSettings: _*)
    .jsSettings(
      coverageExcludedPackages := ".*"
    )
    .dependsOn(`quill-core` % "compile->compile;test->test")

lazy val `quill-sql-jvm` = `quill-sql`.jvm
lazy val `quill-sql-js` = `quill-sql`.js

lazy val `quill-jdbc` =
  (project in file("quill-jdbc"))
    .settings(commonSettings: _*)
    .settings(mimaSettings: _*)
    .settings(
      fork in Test := true,
      libraryDependencies ++= Seq(
        "com.zaxxer"              % "HikariCP"             % "3.2.0",
        "mysql"                   % "mysql-connector-java" % "8.0.12"             % Test,
        "com.h2database"          % "h2"                   % "1.4.197"            % Test,
        "org.postgresql"          % "postgresql"           % "42.2.5"             % Test,
        "org.xerial"              % "sqlite-jdbc"          % "3.25.2"             % Test,
        "com.microsoft.sqlserver" % "mssql-jdbc"           % "6.1.7.jre8-preview" % Test
      )
    )
    .dependsOn(`quill-sql-jvm` % "compile->compile;test->test")

lazy val `quill-spark` =
  (project in file("quill-spark"))
    .settings(commonSettings: _*)
    .settings(mimaSettings: _*)
    .settings(
      crossScalaVersions := Seq("2.11.12"),
      fork in Test := true,
      libraryDependencies ++= Seq(
        "org.apache.spark" %% "spark-sql" % "2.2.0"
      )
    )
    .dependsOn(`quill-sql-jvm` % "compile->compile;test->test")

lazy val `quill-finagle-mysql` =
  (project in file("quill-finagle-mysql"))
    .settings(commonSettings: _*)
    .settings(mimaSettings: _*)
    .settings(
      fork in Test := true,
      libraryDependencies ++= Seq(
        "com.twitter" %% "finagle-mysql" % "18.9.1"
      )
    )
    .dependsOn(`quill-sql-jvm` % "compile->compile;test->test")

lazy val `quill-finagle-postgres` =
  (project in file("quill-finagle-postgres"))
    .settings(commonSettings: _*)
    .settings(mimaSettings: _*)
    .settings(
      fork in Test := true,
      libraryDependencies ++= Seq(
        "io.github.finagle" %% "finagle-postgres" % "0.7.0"
      )
    )
    .dependsOn(`quill-sql-jvm` % "compile->compile;test->test")

lazy val `quill-async` =
  (project in file("quill-async"))
    .settings(commonSettings: _*)
    .settings(mimaSettings: _*)
    .settings(
      fork in Test := true,
      libraryDependencies ++= Seq(
        "com.github.mauricio" %% "db-async-common"  % "0.2.21"
      )
    )
    .dependsOn(`quill-sql-jvm` % "compile->compile;test->test")

lazy val `quill-async-mysql` =
  (project in file("quill-async-mysql"))
    .settings(commonSettings: _*)
    .settings(mimaSettings: _*)
    .settings(
      fork in Test := true,
      libraryDependencies ++= Seq(
        "com.github.mauricio" %% "mysql-async"      % "0.2.21"
      )
    )
    .dependsOn(`quill-async` % "compile->compile;test->test")

lazy val `quill-async-postgres` =
  (project in file("quill-async-postgres"))
    .settings(commonSettings: _*)
    .settings(mimaSettings: _*)
    .settings(
      fork in Test := true,
      libraryDependencies ++= Seq(
        "com.github.mauricio" %% "postgresql-async" % "0.2.21"
      )
    )
    .dependsOn(`quill-async` % "compile->compile;test->test")

lazy val `quill-cassandra` =
  (project in file("quill-cassandra"))
    .settings(commonSettings: _*)
    .settings(mimaSettings: _*)
    .settings(
      fork in Test := true,
      libraryDependencies ++= Seq(
        "com.datastax.cassandra" %  "cassandra-driver-core" % "3.6.0",
        "io.monix"               %% "monix"                 % "2.3.3"
      )
    )
    .dependsOn(`quill-core-jvm` % "compile->compile;test->test")

lazy val `quill-orientdb` =
  (project in file("quill-orientdb"))
      .settings(commonSettings: _*)
      .settings(mimaSettings: _*)
      .settings(
        fork in Test := true,
        libraryDependencies ++= Seq(
          "com.orientechnologies" % "orientdb-graphdb" % "2.2.37"
        )
      )
      .dependsOn(`quill-sql-jvm` % "compile->compile;test->test")

lazy val `tut-sources` = Seq(
  "CASSANDRA.md",
  "README.md"
)

lazy val `tut-settings` = Seq(
  scalacOptions in Tut := Seq(),
  tutSourceDirectory := baseDirectory.value / "target" / "tut",
  tutNameFilter := `tut-sources`.map(_.replaceAll("""\.""", """\.""")).mkString("(", "|", ")").r,
  sourceGenerators in Compile +=
    Def.task {
      `tut-sources`.foreach { name =>
        val source = baseDirectory.value / name
        val file = baseDirectory.value / "target" / "tut" / name
        val str = IO.read(source).replace("```scala", "```tut")
        IO.write(file, str)
      }
      Seq()
    }.taskValue
)

lazy val mimaSettings = MimaPlugin.mimaDefaultSettings ++ Seq(
  mimaPreviousArtifacts := {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, scalaMajor)) if scalaMajor <= 11 =>
        Set(organization.value % s"${name.value}_${scalaBinaryVersion.value}" % "0.5.0")
      case _ =>
        Set()
    }
  }
)

commands += Command.command("checkUnformattedFiles") { st =>
  val vcs = Project.extract(st).get(releaseVcs).get
  val modified = vcs.cmd("ls-files", "--modified", "--exclude-standard").!!.trim.split('\n').filter(_.contains(".scala"))
  if(modified.nonEmpty)
    throw new IllegalStateException(s"Please run `sbt scalariformFormat test:scalariformFormat` and resubmit your pull request. Found unformatted files: ${modified.toList}")
  st
}

def updateReadmeVersion(selectVersion: sbtrelease.Versions => String) =
  ReleaseStep(action = st => {

    val newVersion = selectVersion(st.get(ReleaseKeys.versions).get)

    import scala.io.Source
    import java.io.PrintWriter

    val pattern = """"io.getquill" %% "quill-.*" % "(.*)"""".r

    val fileName = "README.md"
    val content = Source.fromFile(fileName).getLines.mkString("\n")

    val newContent =
      pattern.replaceAllIn(content,
        m => m.matched.replaceAllLiterally(m.subgroups.head, newVersion))

    new PrintWriter(fileName) { write(newContent); close }

    val vcs = Project.extract(st).get(releaseVcs).get
    vcs.add(fileName).!

    st
  })

def updateWebsiteTag =
  ReleaseStep(action = st => {

    val vcs = Project.extract(st).get(releaseVcs).get
    vcs.tag("website", "update website", false).!

    st
  })

lazy val commonSettings = ReleasePlugin.extraReleaseCommands ++ Seq(
  organization := "io.getquill",
  scalaVersion := "2.11.12",
  crossScalaVersions := Seq("2.11.12","2.12.6"),
  libraryDependencies ++= Seq(
    "org.scalamacros" %% "resetallattrs"  % "1.0.0",
    "org.scalatest"   %%% "scalatest"     % "3.0.5"     % Test,
    "ch.qos.logback"  % "logback-classic" % "1.2.3"     % Test,
    "com.google.code.findbugs" % "jsr305" % "3.0.2"     % Provided // just to avoid warnings during compilation
  ),
  EclipseKeys.createSrc := EclipseCreateSrc.Default,
  unmanagedClasspath in Test ++= Seq(
    baseDirectory.value / "src" / "test" / "resources"
  ),
  EclipseKeys.eclipseOutput := Some("bin"),
  scalacOptions ++= Seq(
    "-Xfatal-warnings",
    "-deprecation",
    "-encoding", "UTF-8",
    "-feature",
    "-unchecked",
    "-Yno-adapted-args",
    "-Ywarn-dead-code",
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard",
    "-Xfuture"
  ),
  scalacOptions ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 11)) => 
        Seq("-Xlint", "-Ywarn-unused-import")
      case Some((2, 12)) => 
        Seq("-Xlint:-unused,_", 
            "-Ywarn-unused:imports", 
            "-Ycache-macro-class-loader:last-modified")
      case _ => Seq()
    }
  },
  concurrentRestrictions in Global += Tags.limit(Tags.Test, 1),
  releasePublishArtifactsAction := PgpKeys.publishSigned.value,
  scoverage.ScoverageKeys.coverageMinimum := 96,
  scoverage.ScoverageKeys.coverageFailOnMinimum := false,
  ScalariformKeys.preferences := ScalariformKeys.preferences.value
    .setPreference(AlignParameters, true)
    .setPreference(CompactStringConcatenation, false)
    .setPreference(IndentPackageBlocks, true)
    .setPreference(FormatXml, true)
    .setPreference(PreserveSpaceBeforeArguments, false)
    .setPreference(DoubleIndentConstructorArguments, false)
    .setPreference(RewriteArrowSymbols, false)
    .setPreference(AlignSingleLineCaseStatements, true)
    .setPreference(AlignSingleLineCaseStatements.MaxArrowIndent, 40)
    .setPreference(SpaceBeforeColon, false)
    .setPreference(SpaceInsideBrackets, false)
    .setPreference(SpaceInsideParentheses, false)
    .setPreference(DanglingCloseParenthesis, Force)
    .setPreference(IndentSpaces, 2)
    .setPreference(IndentLocalDefs, false)
    .setPreference(SpacesWithinPatternBinders, true)
    .setPreference(SpacesAroundMultiImports, true),
  publishMavenStyle := true,
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases"  at nexus + "service/local/staging/deploy/maven2")
  },
  pgpSecretRing := file("local.secring.gpg"),
  pgpPublicRing := file("local.pubring.gpg"),
  releasePublishArtifactsAction := PgpKeys.publishSigned.value,
  releaseProcess := {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 11)) =>
        Seq[ReleaseStep](
          checkSnapshotDependencies,
          inquireVersions,
          runClean,
          setReleaseVersion,
          updateReadmeVersion(_._1),
          commitReleaseVersion,
          updateWebsiteTag,
          tagRelease,
          publishArtifacts,
          setNextVersion,
          updateReadmeVersion(_._2),
          commitNextVersion,
          releaseStepCommand("sonatypeReleaseAll"),
          pushChanges
        )
      case Some((2, 12)) =>
        Seq[ReleaseStep](
          checkSnapshotDependencies,
          inquireVersions,
          runClean,
          setReleaseVersion,
          publishArtifacts,
          releaseStepCommand("sonatypeReleaseAll")
        )
      case _ => Seq[ReleaseStep]()
    }
  },
  pomExtra := (
    <url>http://github.com/getquill/quill</url>
    <licenses>
      <license>
        <name>Apache License 2.0</name>
        <url>https://raw.githubusercontent.com/getquill/quill/master/LICENSE.txt</url>
        <distribution>repo</distribution>
      </license>
    </licenses>
    <scm>
      <url>git@github.com:getquill/quill.git</url>
      <connection>scm:git:git@github.com:getquill/quill.git</connection>
    </scm>
    <developers>
      <developer>
        <id>fwbrasil</id>
        <name>Flavio W. Brasil</name>
        <url>http://github.com/fwbrasil/</url>
      </developer>
    </developers>)
)
