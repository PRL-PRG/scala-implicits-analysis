package cz.cvut.fit.prl.scala.implicits.metadata

import cz.cvut.fit.prl.scala.implicits.metadata.Constants.PathSep

case class SLOC(files: String, language: String, blank: String, comment: String, code: String) {
  override def toString = s"$files,$language,$blank,$comment,$code"
}

object SLOC {
  val CsvHeader: Seq[String] = Seq("files", "language", "blank", "comment", "code")
}

case class SourcePath(
    projectId: String,
    moduleId: String,
    scope: String,
    managed: Boolean,
    path: String,
    sloc: SLOC)

object SourcePath {
  val CsvHeader
    : Seq[String] = Seq("project_id", "module_id", "scope", "managed", "path") ++ SLOC.CsvHeader
}

case class Version(
    projectId: String,
    moduleId: String,
    groupId: String,
    artifactId: String,
    version: String,
    platform: String,
    commit: String,
    scalaVersion: String,
    sbtVersion: String,
    updatedScalaVersion: String,
    outputClasspath: String,
    outputTestClasspath: String) {
  val outputClasspaths: Seq[String] = outputClasspath.split(PathSep)
  val outputTestClasspaths: Seq[String] = outputTestClasspath.split(PathSep)
  val output: Seq[String] = outputClasspaths ++ outputTestClasspaths
}

object Version {
  val CsvHeader: Seq[String] = Seq(
    "project_id",
    "module_id",
    "group_id",
    "artifact_id",
    "version",
    "platform",
    "commit",
    "scala_version",
    "sbt_version",
    "updated_scala_version",
    "output_classpath",
    "output_test_classpath")
}

case class Dependency(
    projectId: String,
    moduleId: String,
    groupId: String,
    artifactId: String,
    version: String,
    path: String,
    scope: String,
    transitive: Boolean)

object Dependency {
  val CsvHeader: Seq[String] =
    Seq("project_id", "module_id", "group_id", "artifact_id", "version", "path", "scope", "transitive")
}

case class CleanPath(projectId: String, moduleId: String, path: String)

object CleanPath {
  val CsvHeader: Seq[String] =
    Seq("project_id", "module_id", "path")
}
