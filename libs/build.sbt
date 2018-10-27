val ScalametaVersion = "4.0.0"

ThisBuild / scalaVersion := "2.12.6"
ThisBuild / organization := "cz.cvut.fit.prl.scala.implicits"
ThisBuild / version := "1.0-SNAPSHOT"

// this is for the sbt-buildinfo plugin
resolvers += Resolver.sbtPluginRepo("releases")


val TestLibraries = Seq(
  "org.scalatest" %% "scalatest" % "3.2.0-SNAP10" % Test,
  "org.scalacheck" %% "scalacheck" % "1.13.5" % Test,
)
val LoggingLibraries = Seq(
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.0"
)

// the reason, why we split the project is that there is some bug in either scalapb or buildinfo
// and they do not work well together
// when they are together, the build fails with `BuildInfo is already defined as case class BuildInfo`
// there seem to be some race conditions (sometimes it would simply complain that the buildinfo does not exist)
// it is also super convenient for developing - once can rebuild model with errors in transformation module
lazy val model = (project in file("model")).settings(
  name := "model",
  libraryDependencies ++= Seq(
    "org.scalameta" %% "semanticdb" % ScalametaVersion,
    "com.thesamet.scalapb" %% "scalapb-runtime" % scalapb.compiler.Version.scalapbVersion % "protobuf"
  ),
  libraryDependencies ++= TestLibraries,
  PB.targets in Compile := Seq(
    scalapb.gen(
      flatPackage = true // Don't append filename to package
    ) -> sourceManaged.in(Compile).value
  ),
)

lazy val transformation = (project in file("transformation"))
  .enablePlugins(BuildInfoPlugin)
  .dependsOn(model)
  .settings(
    name := "transformation",
    // Cats relies on improved type inference via the fix for SI-2712,
    // which is not enabled by default.
    scalacOptions += "-Ypartial-unification",

    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-compiler" % scalaVersion.value,
      "com.lihaoyi" %% "pprint" % "0.5.3",
      "com.nrinaudo" %% "kantan.csv-generic" % "0.4.0",
      "org.typelevel" %% "cats-core" % "1.4.0",
      "com.github.pathikrit" %% "better-files" % "3.4.0",
    ),
    libraryDependencies ++= Seq(
      "org.scalameta" %% "scalameta",
      "org.scalameta" %% "metacp",
      "org.scalameta" %% "metap",
      "org.scalameta" %% "symtab",
      "org.scalameta" %% "semanticdb",
      "org.scalameta" % ("metac_" + scalaVersion.value),
      "org.scalameta" % ("interactive_" + scalaVersion.value),
      "org.scalameta" % ("semanticdb-scalac_" + scalaVersion.value),
    ).map(_ % ScalametaVersion),
    libraryDependencies ++= LoggingLibraries,
    // testing dependnecies
    libraryDependencies ++= TestLibraries,
    libraryDependencies ++= Seq(
      "org.scalameta" %% "testkit" % ScalametaVersion % Test,
    ),

    // Support for ammonites
    libraryDependencies += "com.lihaoyi" % "ammonite" % "1.3.2" % "test" cross CrossVersion.full,
    sourceGenerators in Test += Def.task {
      val file = (sourceManaged in Test).value / "amm.scala"
      IO.write(file, """object amm extends App { ammonite.Main.main(args) }""")
      Seq(file)
    }.taskValue,

    buildInfoPackage := "cz.cvut.fit.prl.scala.implicits.utils",
    buildInfoKeys := Seq[BuildInfoKey](
      name,
      version,
      scalaVersion,
      sbtVersion,
      "scalametaVersion" -> ScalametaVersion,
      scalacOptions.in(Test),
      // fullClasspath is impossible since it will need also to recompile the actual BuildInfo
      productDirectories.in(Compile),
      externalDependencyClasspath.in(Test),
      "semanticdbScalacPluginJar" -> (
        System.getProperty("user.home") +
          "/.ivy2/cache/org.scalameta/semanticdb-scalac_" + scalaVersion.value +
          "/jars/" +
          "semanticdb-scalac_" + scalaVersion.value + "-" + ScalametaVersion + ".jar"
        )
    ),

    scalacOptions ++= Seq(
      "-deprecation", "-feature", "-unchecked", "-Xfatal-warnings"
    ),

    parallelExecution.in(Test) := false, // hello, reflection sync!!

    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
  )

lazy val root = (project in file(".")).aggregate(model, transformation)

// this is here so we can extend semnaticdb schema (which we do for merging the raw semanticdb)
// the semanticdb jar does not include the proto file so we cannot use the standard mechanism
// this has to be run manually, I do not know how to make it a dependency for the PB.generate
lazy val downloadSemanticdbProto = taskKey[Unit]("Download semanticdb proto file")
downloadSemanticdbProto := {
  val outputFile = new java.io.File("model/target/protobuf_external/semanticdb.proto")
  if (!outputFile.exists()) {
    if (!outputFile.getParentFile.exists()) {
      outputFile.getParentFile.mkdirs()
    }
    val src = scala.io.Source.fromURL("https://raw.githubusercontent.com/scalameta/scalameta/master/semanticdb/semanticdb/semanticdb.proto")
    val out = new java.io.FileWriter(outputFile)
    out.write(src.mkString)
    out.close()
    println("Downloaded " + outputFile)
  } else {
    println(s"$outputFile has been already downloaded")
  }
}