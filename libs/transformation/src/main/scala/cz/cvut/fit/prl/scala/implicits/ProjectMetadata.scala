package cz.cvut.fit.prl.scala.implicits

import better.files._
import cz.cvut.fit.prl.scala.implicits.extractor.SemanticdbSymbolResolver
import cz.cvut.fit.prl.scala.implicits.symtab.GlobalSymbolTable
import cz.cvut.fit.prl.scala.implicits.utils.Libraries
import kantan.csv._
import kantan.csv.ops._

import scala.meta.internal.{semanticdb => s}
import scala.meta.io.{AbsolutePath, Classpath}

case class ProjectClasspath(
    projectId: String,
    projectName: String,
    path: String
)

case class ProjectVersion(
    projectId: String,
    projectName: String,
    scalaVersion: String,
    sbtVersion: String
)

class ProjectMetadata(path: File) {

  lazy val versionEntries: List[ProjectVersion] = {
    versionFile.path
      .asUnsafeCsvReader[ProjectVersion](rfc.withHeader)
      .toList
  }
  lazy val classpathEntries: List[ProjectClasspath] = {
    classpathFile.path
      .asUnsafeCsvReader[ProjectClasspath](rfc.withHeader)
      .toList
  }
  lazy val semanticdbs: List[s.TextDocument] = {
    val dataFile = (path / Constants.AnalysisDirname / Constants.MergedSemanticdbFilename)

    dataFile.inputStream
      .apply(input => s.TextDocument.streamFromDelimitedInput(input).toList)
  }

  lazy val classpath: Classpath = {
    val absolutePaths = classpathEntries.map(_.path).map(AbsolutePath(_))

    Libraries.JvmBootClasspath ++ Classpath(absolutePaths)
  }
  lazy val symbolTable = GlobalSymbolTable(classpath)
  lazy val resolver = SemanticdbSymbolResolver(semanticdbs, symbolTable)
  val versionFile: File = path / "_analysis_" / "metadata-versions.csv"
  val classpathFile: File = path / "_analysis_" / "metadata-classpath.csv"

}
