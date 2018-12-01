scalaVersion := "2.12.7"
organization := "cz.cvut.fit.prl.scala.corpus"
version := "1.0-SNAPSHOT"
name := "scripts"

libraryDependencies ++= Seq(
  "cz.cvut.fit.prl.scala.implicits" %% "model" % version.value,
  "cz.cvut.fit.prl.scala.implicits" %% "transformation" % version.value,
)
