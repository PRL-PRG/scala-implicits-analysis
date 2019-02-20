lazy val commonSettings = Seq(
  scalaVersion := "2.12.7",
  organization := "org.example",
  version := "1.0-SNAPSHOT"
)

lazy val root = (project in file("."))
  .aggregate(module1, module2)
  .settings(commonSettings)

lazy val module1 = (project in file("module1"))
  .settings(commonSettings)
  .settings(
    name := "module1",
    libraryDependencies += "org.scalameta" %% "semanticdb" % "4.1.0"
  )

lazy val module2 = (project in file("module2"))
  .dependsOn(module1)
  .settings(commonSettings)
  .settings(
    name := "module2",
    libraryDependencies +=  "org.scalatest" %% "scalatest" % "3.2.0-SNAP10" % Test,
    libraryDependencies +=  "org.scalacheck" %% "scalacheck" % "1.13.5" % Test,
  )
