package cz.cvut.fit.prl.scala.implicits

import better.files._
import cz.cvut.fit.prl.scala.implicits.extractor.{LoadingMetadataException, SemanticdbSymbolResolver}
import cz.cvut.fit.prl.scala.implicits.metadata.MetadataFilenames._
import cz.cvut.fit.prl.scala.implicits.metadata._
import cz.cvut.fit.prl.scala.implicits.symtab.{GlobalSymbolTable, SymbolTable}
import cz.cvut.fit.prl.scala.implicits.utils.Libraries
import kantan.csv._
import kantan.csv.generic._
import cats.instances.map._
import cats.instances.list._
import cats.syntax.semigroup._

import scala.annotation.tailrec
import scala.collection.concurrent.TrieMap
import scala.collection.mutable
import scala.meta._
import scala.meta.internal.{semanticdb => s}
import scala.meta.io.{AbsolutePath, Classpath}

case class ClasspathEntry(
    internal: Boolean,
    groupId: String,
    artifactId: String,
    version: String,
    path: String,
    scope: String
)

case class SubProjectMetadata(
    modulesId: String,
    classpathEntries: List[ClasspathEntry],
    sourcepathEntries: List[SourcePath],
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

    def readCSV[T : HeaderDecoder](file: File): List[T] = {
      import kantan.csv.ops._
      file.path.asUnsafeCsvReader(rfc.withHeader).toList
    }

    val versions: List[Version] =
      readCSV[Version](path / AnalysisDirname / VersionsFilename)

    val internalDependencies: List[InternalDependency] =
      readCSV[InternalDependency](path / AnalysisDirname / InternalDependenciesFilename)

    val externalDependencies: List[ExternalDependency] =
      readCSV[ExternalDependency](path / AnalysisDirname / ExternalDependenciesFilename)

    val sourcepathEntries: List[SourcePath] =
      readCSV[SourcePath](path / AnalysisDirname / SourcePathsFilename)

    val versionsMap = versions
    .groupBy(_.moduleId)
    .mapValues {
      case v :: Nil => v
      case x @ vs =>
        throw new Exception(s"Wrong number of projects associated with $x: $vs")
    }

    val sourcepathEntriesMap =
      sourcepathEntries
        .groupBy(_.moduleId)
        .withDefaultValue(Nil)

    val internalClasspathEntriesMap: Map[String, List[ClasspathEntry]] = {
      val dependencyMap = internalDependencies
        .groupBy(_.moduleId)
        .mapValues(_.map(_.dependency))
        .withDefaultValue(Nil)

      @tailrec
      def transitive(consider: List[String], acc: List[String]): List[String] = consider match {
        case Nil => acc.distinct
        case x :: xs => transitive(dependencyMap(x) ++ xs, x :: acc)
      }

      val transitiveDependencies = dependencyMap.mapValues(xs => transitive(xs, Nil))

      transitiveDependencies
      .mapValues { dependencies =>
        dependencies.flatMap { dependency =>
          versionsMap.get(dependency) match {
            case Some(version) =>
              val paths =
                version.outputClasspaths.map(_ -> "compile") ++
                  version.outputTestClasspaths.map(_ -> "test")

              paths.map {
                case (classpath, scope) =>
                  ClasspathEntry(
                    internal = true,
                    version.groupId,
                    version.artifactId,
                    version.version,
                    classpath,
                    scope)
              }
            case None =>
              warnings += new Exception(s"Missing internal dependency $dependency")
              Seq()
          }
        }
      }
    }

    val externalClasspathEntriesMap: Map[String, List[ClasspathEntry]] =
      externalDependencies
        .groupBy(_.moduleId)
        .mapValues { dependencies =>
          dependencies.map { dependency =>
            ClasspathEntry(
              internal = false,
              dependency.groupId,
              dependency.artifactId,
              dependency.version,
              dependency.path,
              dependency.scope)
          }
        }

    // we need to compose the maps, not just concatenate
    val classpathEntriesMap =
      (internalClasspathEntriesMap |+| externalClasspathEntriesMap).withDefaultValue(Nil)

    val semanticdbs: List[s.TextDocument] =
      (path / Constants.MergedSemanticdbFilename).inputStream
        .apply(input => s.TextDocument.streamFromDelimitedInput(input).toList)

    val subProjects: Seq[SubProjectMetadata] = {
      val semanticdbMap: Map[String, List[s.TextDocument]] = {
        val inversePathsMap = sourcepathEntries.groupBy(_.path).mapValues(xs => xs.head.moduleId)
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

      versions.map { version =>
        val projectName = version.moduleId
        SubProjectMetadata(
          projectName,
          classpathEntriesMap(projectName),
          sourcepathEntriesMap(projectName),
          classpathMap(projectName),
          semanticdbMap(projectName)
        )
      }
    }

    val projectId: String = versions.head.projectId

    val scalaVersion: String = versions.head.scalaVersion

    val sbtVersion: String = versions.head.sbtVersion

    (ProjectMetadata(path, projectId, scalaVersion, sbtVersion, subProjects), warnings)
  }
}
