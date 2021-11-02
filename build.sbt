// SPDX-License-Identifier: Apache-2.0

// Provide a managed dependency on X if -DXVersion="" is supplied on the command line.
val defaultVersions = Map("firrtl" -> "1.5-SNAPSHOT")

lazy val baseSettings = Seq(
  name := "treadle",
  organization := "edu.berkeley.cs",
  version := "1.5-SNAPSHOT",
  scalaVersion := "2.12.14",
  crossScalaVersions := Seq("2.13.7", "2.12.13"),
  // enables using control-c in sbt CLI
  cancelable in Global := true,
  resolvers ++= Seq(
    Resolver.sonatypeRepo("snapshots"),
    Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("public")
  ),
  // Ignore dependencies on Berkeley artifacts.
  // scala-steward:off
  libraryDependencies ++= Seq("firrtl").map { dep: String =>
    "edu.berkeley.cs" %% dep % sys.props.getOrElse(dep + "Version", defaultVersions(dep))
  },
  // scala-steward:on
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.2.9" % "test",
    "org.scala-lang.modules" % "scala-jline" % "2.12.1"
  ),
  scalacOptions in Compile ++= Seq(
    "-deprecation",
    "-unchecked",
    "-language:reflectiveCalls",
    "-language:existentials",
    "-language:implicitConversions"
  ),
  // Always target Java8 for maximum compatibility
  javacOptions ++= Seq("-source", "1.8", "-target", "1.8")
)

lazy val assemblySettings = Seq(
  assemblyJarName in assembly := "treadle.jar",
  mainClass in assembly := Some("treadle.TreadleRepl"),
  test in assembly := {}, // Should there be tests?
  assemblyOutputPath in assembly := file("./utils/bin/treadle.jar")
)

lazy val publishSettings = Seq(
  publishMavenStyle := true,
  publishArtifact in Test := false,
  pomIncludeRepository := { x => false },
  pomExtra := <url>http://chisel.eecs.berkeley.edu/</url>
  <licenses>
    <license>
      <name>apache_v2</name>
      <url>https://opensource.org/licenses/Apache-2.0</url>
      <distribution>repo</distribution>
    </license>
  </licenses>
  <developers>
    <developer>
      <id>chick</id>
      <name>Charles Markley</name>
      <url>https://aspire.eecs.berkeley.edu/author/chick/</url>
    </developer>
  </developers>,
  publishTo := {
    val v = version.value
    val nexus = "https://oss.sonatype.org/"
    if (v.trim.endsWith("SNAPSHOT")) {
      Some("snapshots".at(nexus + "content/repositories/snapshots"))
    } else {
      Some("releases".at(nexus + "service/local/staging/deploy/maven2"))
    }
  }
)

lazy val docSettings = Seq(
  scalacOptions in Compile in doc ++= Seq(
    "-deprecation",
    "-Xfatal-warnings",
    "-feature",
    "-diagrams",
    "-diagrams-max-classes",
    "25",
    "-doc-version",
    version.value,
    "-doc-source-url",
    "https://github.com/freechipsproject/treadle/tree/master/€{FILE_PATH}.scala",
    "-sourcepath",
    baseDirectory.value.getAbsolutePath,
    "-unchecked"
  )
)

lazy val treadle = (project in file("."))
  .settings(baseSettings)
  .settings(assemblySettings)
  .settings(publishSettings)
  .settings(docSettings)
  .enablePlugins(BuildInfoPlugin)
  .settings(
    buildInfoPackage := name.value,
    buildInfoUsePackageAsPath := true,
    buildInfoKeys := Seq[BuildInfoKey](buildInfoPackage, version, scalaVersion, sbtVersion)
  )
