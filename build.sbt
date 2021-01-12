// Publish SynapseGrid 1.4.7 (with Scala 2.12 support)
// sbt publishSigned
// to publish for 2.11.8 a couple of lines should be uncommented. In particular, specs2 should be of different version.
lazy val commonSettings = Seq(
  organization := "ru.primetalk",
  version := "1.5.0-SNAPSHOT", //
//  scalaVersion := "2.11.8",
//  libraryDependencies += "org.specs2" %% "specs2" % "3.7" % Test,
//  scalaVersion := "2.12.9",
//  libraryDependencies += "org.specs2" %% "specs2" % "2.4.17" % Test,
  scalaVersion := "2.13.4",
  libraryDependencies += "org.specs2" %% "specs2-core" % "4.7.0" % Test,
  publishArtifact in Test := false,
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % Test,
  scalacOptions += "-deprecation",
)




lazy val core = (project in file("synapse-grid-core")).settings(
  commonSettings,
  name := "synapse-grid-core",
  libraryDependencies += "com.github.mpilquist" %% "simulacrum" % "0.19.0",
  scalacOptions += "-Ymacro-annotations",// required for simulacrum starting from Scala 2.13+
)

lazy val concurrent = (project in file("synapse-grid-concurrent")).settings(
  commonSettings,
  name := "synapse-grid-concurrent"
).dependsOn(core)

lazy val slf4j = (project in file("synapse-grid-slf4j")).settings(
  commonSettings,
  name := "synapse-grid-slf4j",
  libraryDependencies += "org.slf4j" % "slf4j-api" % "1.7.22"
).dependsOn(core)

lazy val akkaVersion = "2.5.24"

lazy val akka = (project in file("synapse-grid-akka")).settings(
  commonSettings,
  name := "synapse-grid-akka",
  libraryDependencies += "com.typesafe.akka" %% "akka-actor" % akkaVersion,
  libraryDependencies += "com.typesafe.akka" %% "akka-remote" % akkaVersion,
  libraryDependencies += "com.typesafe.akka" %% "akka-slf4j" % akkaVersion
).dependsOn(slf4j)

//lazy val rx = (project in file("synapse-grid-rx")).settings(
//  commonSettings,
//  name := "synapse-grid-rx",
//  libraryDependencies += "io.reactivex" %% "rxscala" % "0.26.5"
//).dependsOn(core)

//lazy val examples = (project in file("synapse-grid-examples")).settings(
//  commonSettings,
//  name := "synapse-grid-examples",
//  publishArtifact := false
//).dependsOn(akka, rx)

lazy val root = (project in file(".")).
  aggregate(core, concurrent, slf4j, akka).//, rx, examples).
  settings(
    aggregate in update := false,
    publishArtifact := false
  )

//publishTo := Some("Sonatype Snapshots Nexus" at "https://oss.sonatype.org/content/repositories/snapshots")

//credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")

//<!-- License of your choice -->
//<!-- SCM information. Modify the following URLs -->
//<!-- Developer contact information -->

//licenses := Seq("BSD Software License, 2-clause version" -> url("https://github.com/Primetalk/SynapseGrid/blob/master/LICENSE.md"))

//homepage := Some(url("https://github.com/Primetalk/SynapseGrid"))

// sonatypeProfileName := "ru.primetalk" default - organization.

pomExtra in Global := {
    <url>https://github.com/Primetalk/SynapseGrid</url>
    <licenses>
      <license>
        <name>BSD Software License, 2-clause version</name>
        <url>https://github.com/Primetalk/SynapseGrid/blob/master/LICENSE.md</url>
      </license>
    </licenses>
    <scm>
      <connection>scm:git:github.com/Primetalk/SynapseGrid</connection>
      <developerConnection>scm:git:git@github.com:Primetalk/SynapseGrid</developerConnection>
      <url>github.com/Primetalk/SynapseGrid</url>
    </scm>
    <developers>
      <developer>
        <id>zhizhelev</id>
        <name>Arseniy Zhizhelev</name>
        <url>zhizhelev@primetalk.ru</url>
      </developer>
      <developer>
        <id>nehaev</id>
        <name>Anton Nehaev</name>
        <url>nehaev@primetalk.ru</url>
      </developer>
      <developer>
        <id>popov</id>
        <name>Pavel Popov</name>
        <url>popov@primetalk.ru</url>
      </developer>
    </developers>
}