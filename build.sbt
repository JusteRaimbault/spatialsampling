version in ThisBuild := "0.1-SNAPSHOT"

//scalaVersion := "2.13.1"
scalaVersion := "2.12.10"

organization := "org.openmole.library"

libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6.1"

useGpg := true
publishMavenStyle in ThisBuild := true
publishTo in ThisBuild := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}
lazy val overwriteNonSnapshot = true // use to overwrite when publish non-snapshot if issue during a previous release tentative
publishConfiguration := publishConfiguration.value.withOverwrite(overwriteNonSnapshot)
credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")
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
    setReleaseVersion,
    tagRelease,
    releaseStepCommand("publishSigned"),
    //setNextVersion,
    //commitNextVersion,
    releaseStepCommand("sonatypeRelease")
    //releaseStepCommand("sonatypeReleaseAll")//,
    //pushChanges
)


