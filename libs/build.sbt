// this is for the sbt-buildinfo plugin
resolvers += Resolver.sbtPluginRepo("releases")

lazy val commonSettings = Seq(
  scalaVersion := "2.12.7",
  organization := "cz.cvut.fit.prl.scala.implicits",
  version := "1.0-SNAPSHOT",
)

lazy val root = (project in file("."))
  .aggregate(metadata, model, transformation)
  .settings(commonSettings)
  .settings(name := "libs")

lazy val metadata = (project in file("metadata"))
  .settings(commonSettings)
  .settings(
    name := "metadata",
    crossScalaVersions := Seq("2.10.7", "2.11.12", "2.12.7")
  )

// the reason, why we split the project is that there is some bug in either scalapb or buildinfo
// and they do not work well together
// when they are together, the build fails with `BuildInfo is already defined as case class BuildInfo`
// there seem to be some race conditions (sometimes it would simply complain that the buildinfo does not exist)
// it is also super convenient for developing - once can rebuild model with errors in transformation module
lazy val model = (project in file("model"))
  .dependsOn(metadata)
  .settings(commonSettings)
  .settings(
    name := "model",
    libraryDependencies ++= Seq(
      TypeLevelCats,
      TypeLevelKittens,
      Semanticdb,
      ScalaProtocolBuffers,
      BetterFiles,
      ScalaLogging,
      LogBack,
      ScalaTest,
      ScalaCheck
    ),
    PB.targets in Compile := Seq(
      scalapb.gen(
        flatPackage = true // Don't append filename to package
      ) -> sourceManaged.in(Compile).value
    )
  )

lazy val transformation = (project in file("transformation"))
  .enablePlugins(BuildInfoPlugin)
  .dependsOn(model % "compile->compile;test->test")
  .settings(commonSettings)
  .settings(
    name := "transformation",
    scalacOptions ++= Seq(
      // Cats relies on improved type inference via the fix for SI-2712,
      // which is not enabled by default.
      "-Ypartial-unification",
      "-deprecation",
      "-feature",
      "-unchecked",
      "-Xfatal-warnings"
    ),
    libraryDependencies ++= Seq(
      PPrint,
      KantanCsv,
      TypeLevelCats,
      TypeLevelKittens,
      ScalaLogging,
      LogBack,
      ScalaTest,
      ScalaCheck
    ),
    // do not know how to extract at the bottom, the scalaVersion.value needs ot be in :=
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-compiler" % scalaVersion.value,
      "org.scalameta" %% "scalameta" % ScalametaVersion,
      "org.scalameta" %% "metacp" % ScalametaVersion,
      "org.scalameta" %% "metap" % ScalametaVersion,
      "org.scalameta" %% "symtab" % ScalametaVersion,
      "org.scalameta" %% "semanticdb" % ScalametaVersion,
      "org.scalameta" %% "testkit" % ScalametaVersion % Test,
      "org.scalameta" % ("metac_" + scalaVersion.value) % ScalametaVersion,
      "org.scalameta" % ("interactive_" + scalaVersion.value) % ScalametaVersion,
      "org.scalameta" % ("semanticdb-scalac_" + scalaVersion.value) % ScalametaVersion
    ),
    libraryDependencies += BetterFiles,
    buildInfoPackage := "cz.cvut.fit.prl.scala.implicits.utils",
    buildInfoKeys := Seq[BuildInfoKey](
      name,
      version,
      scalaVersion,
      sbtVersion,
      "scalametaVersion" -> ScalametaVersion,
      scalacOptions.in(Test),
      productDirectories.in(Compile),
      externalDependencyClasspath.in(Test),
      "semanticdbScalacPluginJar" -> (
        System.getProperty("user.home") +
          "/.ivy2/cache/org.scalameta/semanticdb-scalac_" + scalaVersion.value +
          "/jars/" +
          "semanticdb-scalac_" + scalaVersion.value + "-" + ScalametaVersion + ".jar"
      )
    ),
    parallelExecution.in(Test) := false, // hello, reflection sync!!
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
  )

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
    val src = scala.io.Source.fromURL(
      "https://raw.githubusercontent.com/scalameta/scalameta/master/semanticdb/semanticdb/semanticdb.proto")
    val out = new java.io.FileWriter(outputFile)
    out.write(src.mkString)
    out.close()
    println("Downloaded " + outputFile)
  } else {
    println(s"$outputFile has been already downloaded")
  }
}

// Libraries
lazy val ScalametaVersion = "4.1.0"

lazy val TypeLevelCats = "org.typelevel" %% "cats-core" % "1.5.0"
lazy val TypeLevelKittens = "org.typelevel" %% "kittens" % "1.2.0"
lazy val Semanticdb = "org.scalameta" %% "semanticdb" % ScalametaVersion
lazy val ScalaProtocolBuffers = "com.thesamet.scalapb" %% "scalapb-runtime" % scalapb.compiler.Version.scalapbVersion % "protobuf"
lazy val BetterFiles = "com.github.pathikrit" %% "better-files" % "3.4.0"
lazy val KantanCsv = "com.nrinaudo" %% "kantan.csv-generic" % "0.4.0"
lazy val PPrint = "com.lihaoyi" %% "pprint" % "0.5.3"
lazy val ScalaLogging = "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2"
lazy val LogBack = "ch.qos.logback" % "logback-classic" % "1.2.3"

lazy val ScalaTest = "org.scalatest" %% "scalatest" % "3.2.0-SNAP10" % Test
// ScalaTest missing dependency
lazy val ScalaCheck = "org.scalacheck" %% "scalacheck" % "1.13.5" % Test
