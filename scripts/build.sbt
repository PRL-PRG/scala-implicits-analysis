name := "scripts"

organization := "cz.cvut.fit.prl.scala.implicits"

version := "1.0-SNAPSHOT"

scalaVersion := "2.12.8"

libraryDependencies += "com.lihaoyi" % ("ammonite_" + scalaVersion.value) % "1.6.3"
libraryDependencies += "cz.cvut.fit.prl.scala.implicits" %% "tools" % version.value

