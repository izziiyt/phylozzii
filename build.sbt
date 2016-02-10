lazy val commonSettings = Seq(
  organization := "izziiyt",
  version := "0.2.0-SNAPSHOT",
  scalaVersion := "2.10.5",
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "2.1.3" % "test",
    "org.scalanlp" %% "breeze" % "0.11.2",
    "org.scalanlp" %% "breeze-natives" % "0.11.2",
    "org.scalanlp" %% "breeze-viz" % "0.11.2"
  ),
  resolvers ++= Seq(
    "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
    "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/",
    Resolver.sonatypeRepo("public")
  ),
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
  },

  test in assembly := {},

  assemblyJarName in assembly := "phylozzii" + version.value + ".jar",

  testOptions in Test := Seq(Tests.Filter(s => s.endsWith("Test"))),

  scalacOptions ++= Seq(
    "-feature",
    "-optimize",
    "-language:reflectiveCalls",
    "-language:implicitConversions"
  )

)

lazy val root = project.in(file(".")).settings(commonSettings: _*).
  dependsOn(biutil).
  aggregate(fdur, pbls, core).
  settings(aggregate in assembly := false)

lazy val biutil = uri("git://github.com/izziiyt/biutil.git")

lazy val fdur = project.in(file("fdur")).settings(commonSettings: _*).
  settings(
    name := "fdur"
  ).
  dependsOn(biutil)

lazy val pbls = project.in(file("pbls")).settings(commonSettings: _*).
  settings(
    name := "phylozzii.pbls"
  ).
  dependsOn(biutil, fdur)

lazy val core = project.in(file("core")).settings(commonSettings: _*).
  settings(
    name := "core",
    libraryDependencies  ++= Seq(
      "com.github.scopt" %% "scopt" % "3.3.0",
      //"org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
      //"com.typesafe.scala-logging" % "scala-logging-slf4j_2.10" % "2.1.2",
      //"ch.qos.logback" % "logback-classic" % "1.1.3",
      ("org.apache.spark" % "spark-core_2.10" % "1.5.1").
        exclude("org.mortbay.jetty", "servlet-api").
        exclude("com.google.guava","guava").
        exclude("org.apache.hadoop","hadoop-yarn-api").
        exclude("commons-beanutils", "commons-beanutils-phylozzii.core").
        exclude("commons-collections", "commons-collections").
        exclude("commons-logging", "commons-logging").
        exclude("org.spark-project.spark", "unused").
        exclude("com.esotericsoftware.minlog", "minlog")
    )
  ).
  dependsOn(biutil, fdur, pbls)





