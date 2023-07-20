resolvers += Classpaths.sbtPluginReleases

resolvers += "Typesafe repository" at "https://repo.typesafe.com/typesafe/releases/"

addDependencyTreePlugin

addSbtPlugin("org.scalameta"      % "sbt-scalafmt"             % "2.5.0")
addSbtPlugin("org.scoverage"      % "sbt-scoverage"            % "2.0.8")
addSbtPlugin("com.github.sbt"     % "sbt-release"              % "1.1.0")
addSbtPlugin("com.github.sbt"     % "sbt-pgp"                  % "2.2.1")
addSbtPlugin("org.xerial.sbt"     % "sbt-sonatype"             % "3.9.21")
addSbtPlugin("com.typesafe"       % "sbt-mima-plugin"          % "1.1.3")
addSbtPlugin("org.scala-js"       % "sbt-scalajs"              % "1.13.2")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.3.2")
addSbtPlugin("com.etsy"           % "sbt-compile-quick-plugin" % "1.4.0")
addSbtPlugin("dev.zio"            % "zio-sbt-website"          % "0.3.10")
