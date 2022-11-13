import sbt.Keys.testFrameworks

scalaVersion := "2.13.4"
// shadow sbt-scalajs' crossProject and CrossType from Scala.js 0.6.x
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

val sharedSettings = Seq(
  name := "vgutils",
  version := "0.1.8-SNAPSHOT",
  organization := "skac112",
//  scalaVersion := "2.12.4",
  scalaVersion := "2.13.8",
  libraryDependencies += "com.lihaoyi" %%% "utest" % "0.8.0" % "test",
    testFrameworks += new TestFramework("utest.runner.Framework")
)

lazy val vgutils = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Full)
  .in(file("."))
  .settings(sharedSettings)
  .jsSettings(/* ... */) // defined in sbt-scalajs-crossproject
  .jvmSettings( libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.12" % "test")

lazy val vgutilsJVM = vgutils.jvm
lazy val vgutilsJS = vgutils.js