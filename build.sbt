// shadow sbt-scalajs' crossProject and CrossType from Scala.js 0.6.x
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

val sharedSettings = Seq(
  scalaVersion := "2.12.8",
  libraryDependencies += "com.lihaoyi" %%% "utest" % "0.6.3" % "test",
  version := "0.1.0-SNAPSHOT",
  organization := "skac112",
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





//enablePlugins(ScalaJSPlugin)
//scalaVersion in ThisBuild := "2.12.8"
// scalaVersion := "2.12.2"

//lazy val root = project.in(file(".")).
//  aggregate(vgutilsJS, vgutilsJVM).
//  settings(
//    publish := {},
//    publishLocal := {},
//    exportJars := true
//  )
//
//lazy val vgutils = crossProject.in(file(".")).
//  settings(
//    name := "vgutils",
//    version := "0.1.0-SNAPSHOT",
//    organization := "skac",
//    libraryDependencies += "com.lihaoyi" %%% "utest" % "0.6.3" % "test",
//    testFrameworks += new TestFramework("utest.runner.Framework")
//  ).
//  jvmSettings(
//    libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1",
//    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
//    // Add JVM-specific settings here
//  ).
//  jsSettings(
//    // Add JS-specific settings here
//  )
//
//lazy val vgutilsJVM = vgutils.jvm
//lazy val vgutilsJS = vgutils.js
//
////// lazy val root = (project in file(".")).
////  // settings(
//////enablePlugins(ScalaJSPlugin)
////name := "vgutils"
////version := "0.0.1-SNAPSHOT"
////scalaVersion := "2.12.4"
////exportJars := true
////organization := "skac"
////libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1"
////libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
////  // ).
////// enablePlugins(ScalaJSPlugin)
