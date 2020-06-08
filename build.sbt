/*
Copyright 2020 The Regents of the University of California (Regents)

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
 */

def scalacOptionsVersion(scalaVersion: String): Seq[String] = {
  Seq() ++ {
    // If we're building with Scala > 2.11, enable the compile option
    //  switch to support our anonymous Bundle definitions:
    //  https://github.com/scala/bug/issues/10047
    CrossVersion.partialVersion(scalaVersion) match {
      case Some((2, scalaMajor: Long)) if scalaMajor < 12 => Seq()
      case _ => Seq("-Xsource:2.11")
    }
  }
}

def javacOptionsVersion(scalaVersion: String): Seq[String] = {
  Seq() ++ {
    // Scala 2.12 requires Java 8. We continue to generate
    //  Java 7 compatible code for Scala 2.11
    //  for compatibility with old clients.
    CrossVersion.partialVersion(scalaVersion) match {
      case Some((2, scalaMajor: Long)) if scalaMajor < 12 =>
        Seq("-source", "1.7", "-target", "1.7")
      case _ =>
        Seq("-source", "1.8", "-target", "1.8")
    }
  }
}

name := "treadle"

organization := "edu.berkeley.cs"

version := "1.3-SNAPSHOT"

scalaVersion := "2.12.10"

crossScalaVersions := Seq("2.12.10", "2.11.12")

// enables using control-c in sbt CLI
cancelable in Global := true

resolvers ++= Seq(
  Resolver.sonatypeRepo("snapshots"),
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("public")
)

// Assembly

assemblyJarName in assembly := "treadle.jar"

mainClass in assembly := Some("treadle.TreadleRepl")

test in assembly := {} // Should there be tests?

assemblyOutputPath in assembly := file("./utils/bin/treadle.jar")


// Provide a managed dependency on X if -DXVersion="" is supplied on the command line.
val defaultVersions = Map("firrtl" -> "1.4-SNAPSHOT")

// Ignore dependencies on Berkeley artifacts.
// scala-steward:off
libraryDependencies ++= (Seq("firrtl").map {
  dep: String => "edu.berkeley.cs" %% dep % sys.props.getOrElse(dep + "Version", defaultVersions(dep)) })
// scala-steward:on

// sbt 1.2.6 fails with `Symbol 'term org.junit' is missing from the classpath`
// when compiling tests under 2.11.12
// An explicit dependency on junit seems to alleviate this.
libraryDependencies ++= Seq(
  "junit" % "junit" % "4.13" % "test",
  "org.scalatest" %% "scalatest" % "3.1.2" % "test",
  "org.scalacheck" %% "scalacheck" % "1.14.3" % "test",
  "com.github.scopt" %% "scopt" % "3.7.1",
  "org.scala-lang.modules" % "scala-jline" % "2.12.1",
  "org.json4s" %% "json4s-native" % "3.6.8"
)

//javaOptions in run ++= Seq(
    //"-Xms2G", "-Xmx4G", "-XX:MaxPermSize=1024M", "-XX:+UseConcMarkSweepGC")
//)

publishMavenStyle := true

publishArtifact in Test := false
pomIncludeRepository := { x => false }

pomExtra := (<url>http://chisel.eecs.berkeley.edu/</url>
<licenses>
  <license>
    <name>BSD-style</name>
    <url>http://www.opensource.org/licenses/bsd-license.php</url>
    <distribution>repo</distribution>
  </license>
</licenses>
<scm>
  <url>https://github.com/freechipsproject/treadle.git</url>
  <connection>scm:git:github.com/freechipsproject/treadle.git</connection>
</scm>
<developers>
  <developer>
    <id>chick</id>
    <name>Charles Markley</name>
    <url>https://aspire.eecs.berkeley.edu/author/chick/</url>
  </developer>
</developers>)

publishTo := {
  val v = version.value
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT")) {
    Some("snapshots" at nexus + "content/repositories/snapshots")
  }
  else {
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
  }
}

//
// This is for regular compilation
//
scalacOptions in Compile ++= Seq(
  "-deprecation",
  "-unchecked",
  "-language:reflectiveCalls",
  "-language:existentials",
  "-language:implicitConversions",
  "-Ywarn-unused-import" // required by `RemoveUnused` rule
)

//
// This is for doc building
//
scalacOptions in Compile in doc ++= Seq(
  "-deprecation",
  "-Xfatal-warnings",
  "-feature",
  "-diagrams",
  "-diagrams-max-classes", "25",
  "-doc-version", version.value,
  "-doc-source-url", "https://github.com/freechipsproject/treadle/tree/master/€{FILE_PATH}.scala",
  "-sourcepath", baseDirectory.value.getAbsolutePath,
  "-unchecked"
) ++ scalacOptionsVersion(scalaVersion.value)

javacOptions ++= javacOptionsVersion(scalaVersion.value)
