//import sbt.Keys._
//import sbt._
import UnidocKeys._
lazy val commonSettings = Seq(
  organization := "izziiyt",
  version := "0.2.1",
  scalaVersion := "2.11.8",
  name := "phylozzii",
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.0.0" % "test",
    "org.scalanlp" %% "breeze" % "0.12",
    "org.scalanlp" %% "breeze-natives" % "0.12",
    "org.scalanlp" %% "breeze-viz" % "0.12",
    "com.github.scopt" %% "scopt" % "3.5.0"
  ),
  resolvers ++= Seq(
    "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
    "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/",
    Resolver.sonatypeRepo("public")
  ),
  /*assemblyMergeStrategy in assembly := {
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
  },*/
  logLevel in assembly := Level.Error,
  test in assembly := {},
  //logLevel in sbtassembly.Plugin.AssemblyKeys.assembly := Level.Warn,
  assemblyJarName in assembly := name.value + ".jar",
  assemblyOption in assembly := (assemblyOption in assembly).value.copy(includeScala = false),
  //assemblyJarName in assembly := name.value + "-" + version.value + ".jar",

  //assemblyOutputPath in assembly := file("target/"),

  //assemblyPackageDependency in assembly ++= Seq(fdur,pbls,core),

  //projectDependencies in assembly ++= Seq(fdur,pbls,core),

  //mainClass in assembly := Some("phylozzii.core.Main"),

  testOptions in Test := Seq(Tests.Filter(s => s.endsWith("Test"))),

  scalacOptions ++= Seq(
    "-feature",
    "-optimize",
    "-deprecation",
    "-language:reflectiveCalls",
    "-language:implicitConversions"
  )
  //target in Compile in doc := baseDirectory.value.getParentFile / "../izziiyt.github.io/api/"
  /*apiMappings += (
    (classDirectory in biutil).
      ->
      url("https://github.com/izziiyt/izziiyt.github.io/scaladoc/biutil/api/")
    )*/
)

lazy val root = project.in(file(".")).settings(commonSettings: _*).
  settings(unidocSettings: _*).
  settings(
    name := "phylozzii",
    autoAPIMappings := true,
    apiURL := Some(url("https://izziiyt.github.io/scaladoc/phylozzii/2.11/")),
    unidocProjectFilter in (ScalaUnidoc, unidoc) := inAnyProject -- inProjects(biutil)
  ).
  aggregate(fdur, branco, util)

lazy val biutil = RootProject(uri("git://github.com/izziiyt/biutil.git#master"))

lazy val fdur = project.in(file("fdur")).settings(commonSettings: _*).

  settings(
    name := "fdur",
    mainClass in assembly := Some("phylozzii.fdur.Main"),
    libraryDependencies  ++= Seq(
      ("org.apache.spark" % "spark-core_2.11" % "2.0.0").
        exclude("org.mortbay.jetty", "servlet-api").
        //exclude("com.google.guava","guava").
        exclude("org.apache.hadoop","hadoop-yarn-api").
        exclude("commons-beanutils", "commons-beanutils").
        exclude("commons-collections", "commons-collections").
        exclude("org.spark-project.spark", "unused").
        //exclude("com.esotericsoftware.minlog", "minlog").
        exclude("org.slf4j", "jcl-over-slf4j").
        exclude("org.glassfish.hk2.external", "aopalliance-repackaged").
        exclude("org.glassfish.hk2.external", "javax.inject")
    )
  ).
  dependsOn(biutil)

lazy val branco = project.in(file("branco")).settings(commonSettings: _*).
  settings(
    name := "branco",
    mainClass in assembly := Some("phylozzii.branco.Main")

  ).
  dependsOn(biutil, fdur)

lazy val util = project.in(file("util")).settings(commonSettings: _*).
  settings(
    name := "util"
  ).
  dependsOn(biutil, fdur)

