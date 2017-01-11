lazy val commonSettings = Seq(
  organization := "ru.primetalk",
  version := "1.5.0-SNAPSHOT",
  scalaVersion := "2.12.1",
  libraryDependencies += "org.specs2" %% "specs2" % "2.4.17" % Test,
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % Test
)


lazy val core = (project in file("synapse-grid-core")).settings(
  commonSettings,
  name := "synapse-grid-core"
)

lazy val root = (project in file(".")).aggregate(core).settings(
  aggregate in update := false
)
