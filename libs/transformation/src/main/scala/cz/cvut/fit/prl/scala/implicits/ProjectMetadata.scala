package cz.cvut.fit.prl.scala.implicits

import better.files._
import cats.instances.list._
import cats.instances.map._
import cats.syntax.semigroup._
import cz.cvut.fit.prl.scala.implicits.extractor.{
    LoadingMetadataException,
    SemanticdbSymbolResolver
  }
import cz.cvut.fit.prl.scala.implicits.metadata.MetadataFilenames._
import cz.cvut.fit.prl.scala.implicits.metadata._
import cz.cvut.fit.prl.scala.implicits.model.{ClasspathEntry, SourcepathEntry}
import cz.cvut.fit.prl.scala.implicits.extractor.{GlobalSymbolTable, SymbolTable}
import cz.cvut.fit.prl.scala.implicits.utils.Libraries
import cz.cvut.fit.prl.scala.implicits.utils._
import cz.cvut.fit.prl.scala.implicits.Constants._
import cz.cvut.fit.prl.scala.implicits.semanticdb.Semanticdb
import kantan.csv._
import kantan.csv.generic._

import scala.collection.concurrent.TrieMap
import scala.collection.mutable
import scala.meta._
import scala.meta.internal.{semanticdb => s}
import scala.meta.io.{AbsolutePath, Classpath}

case class ModuleMetadata(
    moduleId: String,
    groupId: String,
    artifactId: String,
    version: String,
    scalaVersion: String,
    classpathEntries: List[ClasspathEntry],
    sourcepathEntries: List[SourcepathEntry],
    classpath: Classpath,
    semanticdbs: List[s.TextDocument],
    outputPath: List[String],
    testOutputPath: List[String]
) {
  private val asts: TrieMap[String, Source] = TrieMap()

  lazy val symbolTable: SymbolTable = GlobalSymbolTable(classpath)
  lazy val resolver: SemanticdbSymbolResolver =
    SemanticdbSymbolResolver(semanticdbs, symbolTable, sourcepathEntries.map(_.path))

  def sourcePaths: List[String] = sourcepathEntries.map(_.path)

  def ast(filename: String): Source =
    asts.getOrElseUpdate(filename, semanticdbs.find(_.uri == filename).head.text.parse[Source].get)
}

case class ProjectMetadata(
    path: File,
    projectId: String,
    scalaVersion: String,
    sbtVersion: String,
    modules: Seq[ModuleMetadata]
) {
  lazy val sourcepathEntries: List[SourcepathEntry] =
    modules.foldLeft(List[SourcepathEntry]())(_ ++ _.sourcepathEntries)
}

object ProjectMetadata {
  def apply(path: File): (ProjectMetadata, Seq[Throwable]) = {

    val warnings = mutable.Buffer[Throwable]()

    def readCSV[T: HeaderDecoder](file: File): List[T] = {
      import kantan.csv.ops._
      file.path.asUnsafeCsvReader(rfc.withHeader).toList
    }

    val versions: List[Version] =
      readCSV[Version](path / AnalysisDirname / VersionsFilename)

    val dependencies: List[Dependency] =
      readCSV[Dependency](path / AnalysisDirname / DependenciesFilename)

    val sourcepathEntries: List[SourcePath] =
      readCSV[SourcePath](path / AnalysisDirname / SourcePathsFilename)

    val versionsMap: Map[String, Version] = versions
      .groupBy(_.moduleId)
      .mapValues {
        case v :: Nil => v
        case vs =>
          warnings += new Exception(
            s"There are multiple projects under the same name -- taking the one with existing output: $vs")

          // rough heuristic
          val top =
            vs.map(x => x -> x.output.map(y => if (File(y).exists) 1 else 0).sum).maxBy(_._2)
          top._1
      }

    val sourcepathEntriesMap =
      sourcepathEntries
        .groupBy(_.moduleId)
        .mapValues(xs => xs.map(x => SourcepathEntry(x.path, x.scope, x.managed)))
        .withDefaultValue(Nil)

    val classpathEntriesMap: Map[String, List[ClasspathEntry]] = {
      val internalModules = versions.map(x => (x.groupId, x.artifactId, x.version)).toSet

      dependencies
        .groupBy(_.moduleId)
        .mapValues { dependencies =>
          dependencies.map { dependency =>
            val internal = internalModules.contains(
              (dependency.groupId, dependency.artifactId, dependency.version))

            ClasspathEntry(
              dependency.path,
              dependency.groupId,
              dependency.artifactId,
              dependency.version,
              dependency.scope,
              internal = internal,
              managed = true,
              transitive = dependency.transitive
            )
          }
        }
    }.withDefaultValue(Nil)

    val semanticdbs: List[Semanticdb] =
      (path / Constants.MergedSemanticdbFilename).inputStream
        .apply(input => Semanticdb.streamFromDelimitedInput(input).toList)

    val subProjects: Seq[ModuleMetadata] = {
      val semanticdbMap: Map[String, List[s.TextDocument]] = {
        // an output classpath to moduleId
        val inversePathsMap: Map[String, String] =
          versions.flatMap(x => x.output.map(_ -> x.moduleId)).toMap
        val map = mutable.Map[String, mutable.Buffer[s.TextDocument]]()

        semanticdbs.foreach { sdb =>
          inversePathsMap.collectFirst {
            case (path, project) if sdb.path.startsWith(path) => project
          } match {
            case Some(project) =>
              map.getOrElseUpdate(project, mutable.Buffer()) += sdb.semanticdb
            case None =>
              // it is possible that there are semanticdbs in source paths that we do not know about
              // for example multi-jvm (https://github.com/sbt/sbt-multi-jvm) creates a new scope
              warnings += new LoadingMetadataException(s"No module found for ${sdb.path}")
          }
        }

        map.toMap
          .mapValues(x => x.toList)
          .withDefaultValue(Nil)
      }

      val classpathMap: Map[String, Classpath] =
        classpathEntriesMap
          .map {
            case (name, entries) =>
              val moduleClasspath = entries.map(_.path).distinct.map(AbsolutePath(_))
              val missing = moduleClasspath.filter { x =>
                val file = x.toFile
                file.getName.endsWith(".jar") && !file.exists()
              }

              missing.foreach(x =>
                warnings += new LoadingMetadataException(s"Missing classpath entries: $x"))

              val moduleOutput = versionsMap(name).classpath

              name -> (moduleOutput ++ Classpath(moduleClasspath) ++ Libraries.JvmBootClasspath)
          }
          .withDefault(x => versionsMap(x).classpath ++ Libraries.JvmBootClasspath)

      for {
        version <- versions
        projectId = version.projectId
        moduleId = version.moduleId
        semanticdbs = semanticdbMap(moduleId) if semanticdbs.nonEmpty
        classpathEntries = classpathEntriesMap(moduleId)
        sourcepathEntries = sourcepathEntriesMap(moduleId)
        classpath = classpathMap(moduleId)
      } yield
        ModuleMetadata(
          projectId + "::" + moduleId,
          version.groupId,
          version.artifactId,
          version.version,
          version.scalaVersion,
          classpathEntries,
          sourcepathEntries,
          classpath,
          semanticdbs,
          version.outputClasspaths.toList,
          version.outputTestClasspaths.toList
        )
    }

    val projectId: String = versions.head.projectId

    val scalaVersion: String = versions.head.scalaVersion

    val sbtVersion: String = versions.head.sbtVersion

    (ProjectMetadata(path, projectId, scalaVersion, sbtVersion, subProjects), warnings)
  }
}
