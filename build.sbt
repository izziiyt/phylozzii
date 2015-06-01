import AssemblyKeys._

name := "fdur"

version := "0.1"

organization := "izzii"

scalaVersion := "2.11.6"

libraryDependencies  ++= Seq(
  "org.scalatest" % "scalatest_2.11" % "2.1.3" % "test",
  "org.scalanlp" %% "breeze" % "0.8.1",
  "org.scalanlp" %% "breeze-natives" % "0.8.1",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
  "izzii" %% "biutil" % "0.1"
)

resolvers ++= Seq(
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
)

lazy val buildSettings = Seq(
  version := "0.1",
  organization := "izzii",
  scalaVersion := "2.11.6"
)

val app = (project in file("app")).
  settings(buildSettings: _*).
  settings(assemblySettings: _*).
  settings(
    // your settings here
  )

test in assembly := {}