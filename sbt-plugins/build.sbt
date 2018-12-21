val Version = "1.0-SNAPSHOT"

lazy val sbtplugins = (project in file("."))
    .settings(
      sbtPlugin := true,
      scalaVersion := "2.12.7",
      name := "sbt-plugins",
      organization := "cz.cvut.fit.prl.scala.implicits",
      crossSbtVersions := Vector("0.13.17", "1.0.0"),
      version := Version,
      libraryDependencies += "cz.cvut.fit.prl.scala.implicits" %% "metadata" % Version
    )
