lazy val sbtplugins = (project in file("."))
    .settings(
      sbtPlugin := true,
      scalaVersion := "2.12.7",
      name := "sbt-plugins",
      organization := "cz.cvut.fit.prl.scala-corpus",
      crossSbtVersions := Vector("0.13.17", "1.0.0")
    )
