// SPDX-License-Identifier: Apache-2.0

// Build script for mill 0.6.0
import mill._
import mill.scalalib._
import mill.scalalib.publish._
import coursier.maven.MavenRepository
import $ivy.`com.lihaoyi::mill-contrib-buildinfo:$MILL_VERSION`
import mill.contrib.buildinfo.BuildInfo

object treadle extends mill.Cross[treadleCrossModule]("2.12.13", "2.13.5")

// The following stanza is searched for and used when preparing releases.
// Please retain it.
// Provide a managed dependency on X if -DXVersion="" is supplied on the command line.
val defaultVersions = Map(
  "firrtl" -> "1.5.4"
)

def getVersion(dep: String, org: String = "edu.berkeley.cs") = {
  val version = sys.env.getOrElse(dep + "Version", defaultVersions(dep))
  ivy"$org::$dep:$version"
}

trait CommonModule extends ScalaModule with SbtModule with PublishModule {
  def firrtlModule: Option[PublishModule] = None

  def firrtlIvyDeps = if (firrtlModule.isEmpty)
    Agg(
      getVersion("firrtl")
    )
  else Agg.empty[Dep]

  def moduleDeps = Seq() ++ firrtlModule

  def ivyDeps = super.ivyDeps() ++ firrtlIvyDeps

  def publishVersion = "1.5.4"

  protected def majorVersion = crossVersion.split('.')(1).toInt

  def crossVersion: String

  def scalaVersion = crossVersion

  def repositories() = super.repositories ++ Seq(
    MavenRepository("https://oss.sonatype.org/content/repositories/snapshots"),
    MavenRepository("https://oss.sonatype.org/content/repositories/releases")
  )

  private def javacCrossOptions = majorVersion match {
    case i if i < 12 => Seq("-source", "1.7", "-target", "1.7")
    case _           => Seq("-source", "1.8", "-target", "1.8")
  }

  override def scalacOptions = super.scalacOptions() ++ Agg(
    "-deprecation",
    "-feature"
  )

  override def javacOptions = super.javacOptions() ++ javacCrossOptions

  def pomSettings = PomSettings(
    description = artifactName(),
    organization = "edu.berkeley.cs",
    url = "https://www.chisel-lang.org",
    licenses = Seq(License.`Apache-2.0`),
    versionControl = VersionControl.github("freechipsproject", "treadle"),
    developers = Seq(
      Developer("jackbackrack", "Jonathan Bachrach", "https://eecs.berkeley.edu/~jrb/")
    )
  )
}

class treadleCrossModule(crossVersionValue: String) extends CommonModule with PublishModule with BuildInfo { m =>
  // different scala version shares same sources
  // mill use foo/2.12.13 by default
  override def millSourcePath = super.millSourcePath / os.up / os.up

  def crossVersion = crossVersionValue

  def mainClass = Some("treadle.repl.TreadleReplMain")

  def ivyDeps = super.ivyDeps() ++ Agg(
    ivy"org.scala-lang.modules:scala-jline:2.12.1",
  )

  object test extends Tests {
    private def ivyCrossDeps = majorVersion match {
      case i if i < 12 => Agg(ivy"junit:junit:4.13")
      case _           => Agg()
    }

    def ivyDeps = Agg(
      ivy"org.scalatest::scalatest:3.2.9",
    ) ++ ivyCrossDeps

    def testFrameworks = Seq("org.scalatest.tools.Framework")
  }

  override def buildInfoPackageName = Some("treadle")

  override def buildInfoMembers: T[Map[String, String]] = T {
    Map(
      "buildInfoPackage" -> artifactName(),
      "version" -> publishVersion(),
      "scalaVersion" -> scalaVersion()
    )
  }

  override def generatedSources = T {
    Seq(generatedBuildInfo()._2)
  }

  // make mill publish sbt compatible package
  def artifactName = "treadle"
}
