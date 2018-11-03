package cz.cvut.fit.prl.scala.implicits

import java.nio.file.FileVisitOption

import better.files._
import cz.cvut.fit.prl.scala.implicits.Constants._
import cz.cvut.fit.prl.scala.implicits.extractor.SemanticdbSymbolResolver
import cz.cvut.fit.prl.scala.implicits.symtab.GlobalSymbolTable
import cz.cvut.fit.prl.scala.implicits.utils.{Libraries, SdbLocator}
import kantan.csv._
import kantan.csv.ops._
import kantan.csv.generic._

import scala.collection.concurrent.TrieMap
import scala.meta._
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
//project_id,project_name,scope,kind,path,files,language,blank,comment,code
case class ProjectSourcepath(
    projectId: String,
    projectName: String,
    scope: String,
    kind: String,
    path: String,
    files: Int,
    language: String,
    blank: Int,
    comment: Int,
    code: Int
)

class ProjectMetadata(path: File) {

  def mergeSemanticdbs(): Unit = {
    mergedSemanticdbFile.outputStream.apply { output =>
      new SdbLocator(path.path)
        .exclude(Constants.ExcludedDirs)
        .options(FileVisitOption.FOLLOW_LINKS)
        .run {
          case (_, db) => db.documents.foreach(_.writeDelimitedTo(output))
        }
    }
  }

  val versionFile: File = metadataFile(VersionsFilename)
  val classpathsFile: File = metadataFile(ClasspathsFilename)
  val sourcepathsFile: File = metadataFile(SourcepathsFilename)
  val mergedSemanticdbFile: File = metadataFile(PerProjectMergedSemanticdbFilename)
  private val asts: TrieMap[String, Source] = TrieMap()

  lazy val versionEntries: List[ProjectVersion] = {
    versionFile.path
      .asUnsafeCsvReader[ProjectVersion](rfc.withHeader)
      .toList
  }

  lazy val classpathEntries: List[ProjectClasspath] = {
    classpathsFile.path
      .asUnsafeCsvReader[ProjectClasspath](rfc.withHeader)
      .toList
  }

  lazy val sourcepathEntries: List[ProjectSourcepath] = {
    sourcepathsFile.path
      .asUnsafeCsvReader[ProjectSourcepath](rfc.withHeader)
      .toList
  }

  lazy val semanticdbs: List[s.TextDocument] = {
    if (!mergedSemanticdbFile.exists) {
      mergeSemanticdbs()
    }

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

  def metadataFile(filename: String): File = path / AnalysisDirname / filename

  def ast(filename: String): Source =
    asts.getOrElseUpdate(
      filename,
      semanticdbs.find(_.uri == filename).head.text.parse[Source].get)
}
