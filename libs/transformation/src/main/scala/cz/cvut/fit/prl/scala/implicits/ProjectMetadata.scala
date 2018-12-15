package cz.cvut.fit.prl.scala.implicits

import better.files._
import cz.cvut.fit.prl.scala.implicits.Constants._
import cz.cvut.fit.prl.scala.implicits.extractor.{
  LoadingMetadataException,
  SemanticdbSymbolResolver
}
import cz.cvut.fit.prl.scala.implicits.symtab.{GlobalSymbolTable, SymbolTable}
import cz.cvut.fit.prl.scala.implicits.utils.Libraries
import kantan.csv._
import kantan.csv.ops._
import kantan.csv.generic._

import scala.collection.concurrent.TrieMap
import scala.collection.mutable
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

case class SubProjectMetadata(
    projectName: String,
    classpathEntries: List[ClasspathEntry],
    sourcepathEntries: List[SourcepathEntry],
    classpath: Classpath,
    semanticdbs: List[s.TextDocument]
) {
  private val asts: TrieMap[String, Source] = TrieMap()

  lazy val symbolTable: SymbolTable = GlobalSymbolTable(classpath)
  lazy val resolver: SemanticdbSymbolResolver = SemanticdbSymbolResolver(semanticdbs, symbolTable)

  def ast(filename: String): Source =
    asts.getOrElseUpdate(filename, semanticdbs.find(_.uri == filename).head.text.parse[Source].get)
}

case class ProjectMetadata(
    path: File,
    projectId: String,
    scalaVersion: String,
    sbtVersion: String,
    subProjects: Seq[SubProjectMetadata]
)

object ProjectMetadata {
  def apply(path: File): (ProjectMetadata, Seq[Throwable]) = {

    val warnings = mutable.Buffer[Throwable]()

    val versionEntries: List[ProjectVersion] = {
      (path / VersionsFilename).path
        .asUnsafeCsvReader[ProjectVersion](rfc.withHeader)
        .toList
    }

    val classpathEntries: List[ClasspathEntry] = {
      (path / ClasspathFilename).path
        .asUnsafeCsvReader[ClasspathEntry](rfc.withHeader)
        .toList
    }

    val sourcepathEntries: List[SourcepathEntry] = {
      (path / SourcepathFilename).path
        .asUnsafeCsvReader[SourcepathEntry](rfc.withHeader)
        .toList
    }

    val sourcepathEntriesMap =
      sourcepathEntries
        .groupBy(_.projectName)
        .withDefaultValue(Nil)

    val classpathEntriesMap =
      classpathEntries
        .groupBy(_.projectName)
        .withDefaultValue(Nil)

    val semanticdbs: List[s.TextDocument] =
      (path / MergedSemanticdbFilename).inputStream
        .apply(input => s.TextDocument.streamFromDelimitedInput(input).toList)

    val subProjects: Seq[SubProjectMetadata] = {
      val semanticdbMap: Map[String, List[s.TextDocument]] = {
        val inversePathsMap = sourcepathEntries.groupBy(_.path).mapValues(xs => xs.head.projectName)
        val map = mutable.Map[String, mutable.Buffer[s.TextDocument]]()
        semanticdbs.foreach { sdb =>
          inversePathsMap.collectFirst {
            case (path, project) if sdb.uri.startsWith(path) => project
          } match {
            case Some(project) =>
              map.getOrElseUpdate(project, mutable.Buffer()) += sdb
            case None =>
              // it is possible that there are semanticdbs in source paths that we do not know about
              // for example multi-jvm (https://github.com/sbt/sbt-multi-jvm) creates a new scope
              warnings += new LoadingMetadataException(s"No source entry for ${sdb.uri}")
          }
        }

        map.toMap
          .mapValues(x => x.toList)
          .withDefaultValue(Nil)
      }

      val classpathMap = classpathEntriesMap
        .mapValues { entries =>
          val absolutePaths = entries.map(_.path).distinct.map(AbsolutePath(_))
          val missing = absolutePaths.filter { x =>
            val file = x.toFile
            file.getName.endsWith(".jar") && !file.exists()
          }

          missing.foreach(x =>
            warnings += new LoadingMetadataException(s"Missing classpath entries: $x"))

          Libraries.JvmBootClasspath ++ Classpath(absolutePaths)
        }
        .withDefaultValue(Classpath(Nil))

      versionEntries.map { version =>
        val projectName = version.projectName
        SubProjectMetadata(
          projectName,
          classpathEntriesMap(projectName),
          sourcepathEntriesMap(projectName),
          classpathMap(projectName),
          semanticdbMap(projectName)
        )
      }
    }

    val projectId: String = versionEntries.head.projectId

    val scalaVersion: String = versionEntries.head.scalaVersion

    val sbtVersion: String = versionEntries.head.sbtVersion

    (ProjectMetadata(path, projectId, scalaVersion, sbtVersion, subProjects), warnings)
  }
}
