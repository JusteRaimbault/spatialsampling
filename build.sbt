
scalaVersion in ThisBuild := "2.13.2"
crossScalaVersions in ThisBuild := Seq("2.12.11", "2.13.2")


organization := "org.openmole.library"

libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6.1"

useGpg := true
publishMavenStyle in ThisBuild := true
publishTo in ThisBuild := sonatypePublishToBundle.value

lazy val overwriteNonSnapshot = true // use to overwrite when publish non-snapshot if issue during a previous release tentative
publishConfiguration := publishConfiguration.value.withOverwrite(overwriteNonSnapshot)

licenses in ThisBuild := Seq("Affero GPLv3" -> url("http://www.gnu.org/licenses/"))
homepage in ThisBuild := Some(url("https://github.com/openmole/spatialsampling"))
scmInfo in ThisBuild := Some(ScmInfo(url("https://github.com/openmole/spatialsampling.git"), "scm:git:git@github.com:openmole/spatialsampling.git"))
pomExtra in ThisBuild :=
  <developers>
    <developer>
      <id>justeraimbault</id>
      <name>Juste Raimbault</name>
    </developer>
  </developers>


sonatypeProfileName := "org.openmole"

import sbtrelease.ReleasePlugin.autoImport.ReleaseTransformations._

releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  runClean,
  runTest,
  setReleaseVersion,
  tagRelease,
  releaseStepCommandAndRemaining("+publishSigned"),
  releaseStepCommand("sonatypeBundleRelease"),
  setNextVersion,
  commitNextVersion,
  //releaseStepCommand("sonatypeReleaseAll"),
  pushChanges
)

