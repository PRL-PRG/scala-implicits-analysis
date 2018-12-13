package cz.cvut.fit.prl.scala.implicits

import better.files._
import cz.cvut.fit.prl.scala.implicits.Constants._
import cz.cvut.fit.prl.scala.implicits.extractor.SemanticdbSymbolResolver
import cz.cvut.fit.prl.scala.implicits.symtab.GlobalSymbolTable
import cz.cvut.fit.prl.scala.implicits.utils.Libraries
import kantan.csv._
import kantan.csv.ops._
import kantan.csv.generic._

import scala.collection.concurrent.TrieMap
import scala.meta._
import scala.meta.internal.{semanticdb => s}
import scala.meta.io.{AbsolutePath, Classpath}

case class ClasspathEntry(
    projectId: String,
    projectName: String,
    path: String,
    scope: String
)

case class ProjectVersion(
    projectId: String,
    projectName: String,
    commit: String,
    scalaVersion: String,
    sbtVersion: String
)

case class SourcepathEntry(
    projectId: String,
    projectName: String,
    scope: String,
    managed: Boolean,
    path: String,
    files: Int,
    language: String,
    blank: Int,
    comment: Int,
    code: Int
)

class ProjectMetadata(path: File) {

  private val versionFile = metadataFile(VersionsFilename)
  private val classpathFile = metadataFile(ClasspathFilename)
  private val sourcepathFile = metadataFile(SourcepathFilename)
  private val mergedSemanticdbFile = metadataFile(MergedSemanticdbFilename)
  private val asts: TrieMap[String, Source] = TrieMap()

  lazy val versionEntries: List[ProjectVersion] = {
    versionFile.path
      .asUnsafeCsvReader[ProjectVersion](rfc.withHeader)
      .toList
  }

  lazy val classpathEntries: List[ClasspathEntry] = {
    classpathFile.path
      .asUnsafeCsvReader[ClasspathEntry](rfc.withHeader)
      .toList
  }

  lazy val sourcepathEntries: List[SourcepathEntry] = {
    sourcepathFile.path
      .asUnsafeCsvReader[SourcepathEntry](rfc.withHeader)
      .toList
  }

  lazy val semanticdbs: List[s.TextDocument] = {
    mergedSemanticdbFile.inputStream
      .apply(input => s.TextDocument.streamFromDelimitedInput(input).toList)
  }

  lazy val classpath: Classpath = {
    val absolutePaths = classpathEntries.map(_.path).distinct.map(AbsolutePath(_))
    val missing = absolutePaths.filter { x =>
      val file = x.toFile
      file.getName.endsWith(".jar") && !file.exists()
    }

    // TODO: should return Try[Classpath]
    if (missing.nonEmpty) {
      throw new Exception(s"Missing classpath entries: ${missing.mkString(":")}")
    }

    Libraries.JvmBootClasspath ++ Classpath(absolutePaths)
  }

  lazy val symbolTable = GlobalSymbolTable(classpath)

  lazy val resolver = SemanticdbSymbolResolver(semanticdbs, symbolTable)

  lazy val projectId: String = versionEntries.head.projectId

  lazy val scalaVersion: String = versionEntries.head.scalaVersion

  lazy val sbtVersion: String = versionEntries.head.sbtVersion

  private def metadataFile(filename: String): File = path / filename

  def ast(filename: String): Source =
    asts.getOrElseUpdate(
      filename,
      semanticdbs.find(_.uri == filename).head.text.parse[Source].get)
}
