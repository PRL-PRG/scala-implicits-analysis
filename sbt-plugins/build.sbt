ThisBuild / scalaVersion := "2.12.7"
ThisBuild / version := "1.0-SNAPSHOT"
ThisBuild / organization := "cz.cvut.fit.prl.scala.implicits"

lazy val metadata = (project in file("metadata"))
  .settings(
    name := "metadata"
  )

lazy val plugins = (project in file("plugins"))
  .dependsOn(metadata)
  .settings(
    sbtPlugin := true,
    name := "plugins",
    crossSbtVersions := Vector("0.13.17", "1.0.0")
  )

lazy val root = (project in file("."))
  .aggregate(metadata, plugins)
  .settings(name := "sbt-plugins")
