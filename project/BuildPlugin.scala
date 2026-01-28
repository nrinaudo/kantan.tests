import sbt.*, Keys.*
import sbt.plugins.{JvmPlugin, SbtPlugin}
import sbt.ScriptedPlugin.autoImport.*
import sbtrelease.ReleasePlugin, ReleasePlugin.autoImport.*, ReleaseTransformations.*, ReleaseKeys.*
import xerial.sbt.Sonatype.SonatypeKeys.*

object BuildPlugin extends AutoPlugin {
  override def trigger = allRequirements

  override def requires = JvmPlugin && ReleasePlugin

  override lazy val projectSettings = baseSettings ++ releaseSettings

  override def globalSettings = addCommandAlias(
    "validate",
    ";clean;scalafmtCheck;Test / scalafmtCheck;scalafmtSbtCheck;test"
  )

  def baseSettings: Seq[sbt.Def.Setting[_]] =
    Seq(
      publishConfiguration := publishConfiguration.value.withOverwrite(true),
      organization         := "com.nrinaudo",
      organizationHomepage := Some(url("https://nrinaudo.github.io")),
      organizationName     := "Nicolas Rinaudo",
      startYear            := Some(2026),
      scalaVersion         := "3.8.1",
      licenses             := Seq("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0.html")),
      homepage             := Some(url(s"https://nrinaudo.github.io/kantan.tests")),
      developers           := List(
        Developer("nrinaudo", "Nicolas Rinaudo", "nicolas@nrinaudo.com", url("https://twitter.com/nicolasrinaudo"))
      ),
      scmInfo := Some(
        ScmInfo(
          url(s"https://github.com/nrinaudo/kantan.tests"),
          s"scm:git:git@github.com:nrinaudo/kantan.tests.git"
        )
      ),
      scalacOptions ++= Seq(
        "-source",
        "future",
        "-Xkind-projector:underscores",
        "-deprecation",
        "-experimental",
        "-Yexplicit-nulls",
        "-language:experimental.captureChecking"
      ),

      publishTo := sonatypePublishToBundle.value
    )

  def releaseSettings: Seq[Setting[_]] =
    Seq(
      releaseCrossBuild := true,
      releaseProcess    := Seq[ReleaseStep](
        checkSnapshotDependencies,
        inquireVersions,
        runClean,
        setReleaseVersion,
        commitReleaseVersion,
        tagRelease,
        releaseStepCommand("publishSigned"),
        releaseStepCommand("sonatypeBundleRelease"),
        setNextVersion,
        commitNextVersion,
        pushChanges
      )
    )
}
