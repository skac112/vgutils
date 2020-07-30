// shadow sbt-scalajs' crossProject and CrossType from Scala.js 0.6.x
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

val sharedSettings = Seq(
  name := "vgutils",
  version := "0.1.3-SNAPSHOT",
  organization := "skac112",
  scalaVersion := "2.12.4",
  libraryDependencies += "com.lihaoyi" %%% "utest" % "0.6.3" % "test"
)

lazy val vgutils = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Full)
  .in(file("."))
  .settings(sharedSettings)
  .jsSettings(/* ... */) // defined in sbt-scalajs-crossproject
  .jvmSettings( libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test")

lazy val vgutilsJVM = vgutils.jvm
lazy val vgutilsJS = vgutils.js