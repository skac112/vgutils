// lazy val root = (project in file(".")).
  // settings(
enablePlugins(ScalaJSPlugin)
name := "vgutils"
version := "0.0.1-SNAPSHOT"
scalaVersion := "2.12.4"
exportJars := true
organization := "skac"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
  // ).
// enablePlugins(ScalaJSPlugin)
