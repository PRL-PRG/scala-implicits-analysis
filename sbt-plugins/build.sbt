sbtPlugin := true
scalaVersion := "2.12.6"

name := "sbt-plugins"
organization := "cz.cvut.fit.prl.scala-corpus"

libraryDependencies ++= Seq(
  "com.jsuereth" %% "scala-arm" % "2.0"
)
