import ammonite.ops._
import ammonite.ops.ImplicitWd._
import mill._
import mill.scalalib._
import mill.scalalib.publish._
import mill.eval.Evaluator

import $file.CommonBuild

// An sbt layout with src in the top directory.
trait CrossUnRootedSbtModule extends CrossSbtModule {
  override def millSourcePath = super.millSourcePath / ammonite.ops.up
}

trait CommonModule extends CrossUnRootedSbtModule with PublishModule {
  def publishVersion = "1.1-SNAPSHOT"

  def pomSettings = PomSettings(
    description = artifactName(),
    organization = "edu.berkeley.cs",
    url = "https://github.com/freechipsproject/treadle.git",
    licenses = Seq(License.`BSD-3-Clause`),
    versionControl = VersionControl.github("freechipsproject", "treadle"),
    developers = Seq(
      Developer("chick",    "Charles Markley",      "https://aspire.eecs.berkeley.edu/author/chick/")
    )
  )

  override def scalacOptions = Seq(
    "-deprecation",
    "-explaintypes",
    "-feature", "-language:reflectiveCalls",
    "-unchecked",
    "-Xcheckinit",
    "-Xlint:infer-any",
    "-Xlint:missing-interpolator"
  ) ++ CommonBuild.scalacOptionsVersion(crossScalaVersion)

  override def javacOptions = CommonBuild.javacOptionsVersion(crossScalaVersion)
}

val crossVersions = Seq("2.12.6", "2.11.12")

// Make this available to external tools.
object treadle extends Cross[TreadleModule](crossVersions: _*) {
  def defaultVersion(ev: Evaluator[Any]) = T.command{
    println(crossVersions.head)
  }

  def compile = T{
    treadle(crossVersions.head).compile()
  }

  def jar = T{
    treadle(crossVersions.head).jar()
  }

  def test = T{
    treadle(crossVersions.head).test.test()
  }

  def publishLocal = T{
    treadle(crossVersions.head).publishLocal()
  }

  def docJar = T{
    treadle(crossVersions.head).docJar()
  }
}

// Provide a managed dependency on X if -DXVersion="" is supplied on the command line.
val defaultVersions = Map("firrtl" -> "1.2-SNAPSHOT")

def getVersion(dep: String, org: String = "edu.berkeley.cs") = {
  val version = sys.env.getOrElse(dep + "Version", defaultVersions(dep))
  ivy"$org::$dep:$version"
}

class TreadleModule(val crossScalaVersion: String) extends CommonModule {
  override def artifactName = "treadle"

  def chiselDeps = Agg("firrtl").map { d => getVersion(d) }

  override def ivyDeps = Agg(
    ivy"org.scala-lang.modules:scala-jline:2.12.1",
    ivy"org.json4s::json4s-native:3.5.3"
  ) ++ chiselDeps

  object test extends Tests {
    override def ivyDeps = Agg(
      ivy"org.scalatest::scalatest:3.0.1",
      ivy"org.scalacheck::scalacheck:1.13.4"
    )
    def testFrameworks = Seq("org.scalatest.tools.Framework")
  }

}
