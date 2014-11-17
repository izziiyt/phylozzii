name := "fdur"

version := "0.1"

organization := "izzii"

scalaVersion := "2.10.3"

libraryDependencies  ++= Seq(
  "org.scalatest" % "scalatest_2.10" % "2.0" % "test",
  "com.github.wookietreiber" %% "scala-chart" % "latest.integration",
  "org.scalanlp" %% "breeze" % "0.8.1",
  "org.scalanlp" %% "breeze-natives" % "0.8.1"
)

resolvers ++= Seq(
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
)

lazy val buildSettings = Seq(
  version := "0.1",
  organization := "izzii",
  scalaVersion := "2.10.3"
)