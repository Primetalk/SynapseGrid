ThisBuild / organization := "ru.primetalk"
ThisBuild / organizationName := "SynapseGrid"
ThisBuild / organizationHomepage := Some(url("http://synapse-grid.primetalk.ru/"))
ThisBuild / isSnapshot := false

ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/Primetalk/SynapseGrid"),
    "scm:https://github.com/Primetalk/SynapseGrid.git"
  )
)
ThisBuild / developers := List(
  Developer(
    id    = "Primetalk",
    name  = "Arseniy Zhizhelev",
    email = "zhizhelev@primetalk.ru",
    url   = url("https://primetalk.github.io/")
  ),
  Developer(
    id    = "nehaev",
    name  = "Anton Nehaev",
    email = "nehaev@primetalk.ru",
    url   = url("https://primetalk.github.io/")
  ),
  Developer(
    id    = "popov",
    name  = "Pavel Popov",
    email = "popov@primetalk.ru",
    url   = url("https://primetalk.github.io/")
  ),
)

ThisBuild / description := "SynapseGrid is a framework for constructing dynamic low latency data flow graph systems."
ThisBuild / licenses := List("BSD Software License, 2-clause version" -> url("https://github.com/Primetalk/SynapseGrid/blob/master/LICENSE.md"))
ThisBuild / homepage := Some(url("https://github.com/Primetalk/SynapseGrid"))

// Remove all additional repository other than Maven Central from POM
//ThisBuild / pomIncludeRepository := { _ => false }
ThisBuild / publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}
//ThisBuild / publishMavenStyle := true

pgpSecretRing := pgpPublicRing.value

usePgpKeyHex("855c7687")

