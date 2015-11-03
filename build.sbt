
//import sbt.Keys._
scalaVersion := "2.10.5"
version := "0.1.0"
name := "fdur"
libraryDependencies  ++= Seq(
  "org.scalatest" % "scalatest_2.10" % "2.1.3" % "test",
  "org.scalanlp" %% "breeze" % "0.11.2",
  "org.scalanlp" %% "breeze-natives" % "0.11.2",
  "org.scalanlp" %% "breeze-viz" % "0.11.2",
  //"org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
  //"com.typesafe.scala-logging" % "scala-logging-slf4j_2.10" % "2.1.2",
  //"ch.qos.logback" % "logback-classic" % "1.1.3",
  "izzii" %% "biutil" % "0.1",
  ("org.apache.spark" % "spark-core_2.10" % "1.5.1").
    exclude("org.mortbay.jetty", "servlet-api").
    exclude("com.google.guava","guava").
    exclude("org.apache.hadoop","hadoop-yarn-api").
    exclude("commons-beanutils", "commons-beanutils-core").
    exclude("commons-collections", "commons-collections").
    exclude("commons-logging", "commons-logging").
    exclude("org.spark-project.spark", "unused").
    exclude("com.esotericsoftware.minlog", "minlog"),
    "com.esotericsoftware.kryo" % "kryo" % "2.24.0"
)

//scalacOptions += "-feature"

resolvers ++= Seq(
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
)

assemblyMergeStrategy in assembly := {
  case PathList("META-INF") => MergeStrategy.first
  case x if Assembly.isConfigFile(x) =>
    MergeStrategy.concat
  case PathList(ps @ _*) if Assembly.isReadme(ps.last) || Assembly.isLicenseFile(ps.last) =>
    MergeStrategy.rename
  case PathList("META-INF", xs @ _*) =>
    xs map {_.toLowerCase} match {
      case ("manifest.mf" :: Nil) | ("index.list" :: Nil) | ("dependencies" :: Nil) =>
        MergeStrategy.discard
      case ps @ (y :: ys) if ps.last.endsWith(".sf") || ps.last.endsWith(".dsa") =>
        MergeStrategy.discard
      case "plexus" :: ys =>
        MergeStrategy.discard
      case "services" :: ys =>
        MergeStrategy.filterDistinctLines
      case ("spring.schemas" :: Nil) | ("spring.handlers" :: Nil) =>
        MergeStrategy.filterDistinctLines
      case _ => MergeStrategy.first
    }
  case _ => MergeStrategy.first
}

test in assembly := {}

testOptions in Test := Seq(Tests.Filter(s => s.endsWith("Test")))
