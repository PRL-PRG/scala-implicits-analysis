val circeVersion = "0.10.0"

lazy val commonSettings = Seq(
  scalaVersion := "2.12.7",
  organization := "org.example2",
  version := "1.0-SNAPSHOT",
  scalacOptions += "-Ystatistics:typer",
  libraryDependencies ++= Seq(
    "io.circe" %% "circe-core",
    "io.circe" %% "circe-generic",
    "io.circe" %% "circe-parser"
  ).map(_ % circeVersion)
)

lazy val root = (project in file("."))
  .aggregate(manual, semiautomatic, automatic, random)
  .settings(commonSettings)

lazy val manual = (project in file("manual"))
  .settings(commonSettings)
  .settings(
    name := "manual",
  )

lazy val semiautomatic = (project in file("semiautomatic"))
  .settings(commonSettings)
  .settings(
    name := "semiautomatic",
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-generic",
    ).map(_ % circeVersion)
  )

lazy val automatic = (project in file("automatic"))
  .settings(commonSettings)
  .settings(
    name := "automatic",
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-generic",
    ).map(_ % circeVersion)
  )

lazy val random = (project in file("random"))
  .settings(commonSettings)
  .settings(
    name := "random",
  )
