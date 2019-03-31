package cz.cvut.fit.prl.scala.implicits

import java.nio.file.Path

import better.files._
import cats.instances.list._
import cats.instances.map._
import cats.syntax.semigroup._
import cz.cvut.fit.prl.scala.implicits.extractor.{LoadingMetadataException, SemanticdbSymbolResolver}
import cz.cvut.fit.prl.scala.implicits.metadata.MetadataFilenames._
import cz.cvut.fit.prl.scala.implicits.metadata._
import cz.cvut.fit.prl.scala.implicits.model.{ClasspathEntry, SourcepathEntry}
import cz.cvut.fit.prl.scala.implicits.extractor.{GlobalSymbolTable, SymbolTable}
import cz.cvut.fit.prl.scala.implicits.utils.Libraries
import cz.cvut.fit.prl.scala.implicits.utils._
import kantan.csv._
import kantan.csv.generic._

import scala.collection.concurrent.TrieMap
import scala.collection.mutable
import scala.meta._
import scala.meta.internal.{semanticdb => s}
import scala.meta.io.{AbsolutePath, Classpath}

case class ModuleMetadata(
    projectBasePath: Path,
    moduleId: String,
    groupId: String,
    artifactId: String,
    version: String,
    commit: String,
    scalaVersion: String,
    classpathEntries: List[ClasspathEntry],
    sourcepathEntries: List[SourcepathEntry],
    classpath: Classpath,
    semanticdbs: List[s.TextDocument],
    outputPath: List[String],
    testOutputPath: List[String]
) {
  private val asts: TrieMap[String, Source] = TrieMap()

  lazy val symbolTable: SymbolTable = new GlobalSymbolTable(classpath, projectBasePath)
  lazy val resolver: SemanticdbSymbolResolver =
    SemanticdbSymbolResolver(semanticdbs, symbolTable, sourcepathEntries.map(_.path))

  def sourcePaths: List[String] = sourcepathEntries.map(_.path)

  def ast(filename: String): Source =
    asts.getOrElseUpdate(filename, semanticdbs.find(_.uri == filename).head.text.parse[Source].get)
}

case class ProjectMetadata(
    projectPath: Path,
    projectId: String,
    scalaVersion: String,
    sbtVersion: String,
    modules: Seq[ModuleMetadata]
) {
  lazy val sourcepathEntries: List[SourcepathEntry] =
    modules.foldLeft(List[SourcepathEntry]())(_ ++ _.sourcepathEntries)
}

object ProjectMetadata {
  type ModuleId = String

  def apply(projectPath: File): (ProjectMetadata, Seq[Throwable]) = {

    val warnings = mutable.Buffer[Throwable]()

    def readCSV[T: HeaderDecoder](file: File): List[T] = {
      import kantan.csv.ops._
      file.path.asUnsafeCsvReader(rfc.withHeader).toList
    }

    val modules: List[Module] =
      readCSV[Module](projectPath / AnalysisDirname / ModulesFilename)

    val dependencies: List[Dependency] =
      readCSV[Dependency](projectPath / AnalysisDirname / DependenciesFilename)

    val sourcepathEntries: List[SourcePath] =
      readCSV[SourcePath](projectPath / AnalysisDirname / SourcePathsFilename)

    val modulesMap: Map[ModuleId, Module] = modules
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

    val sourcepathEntriesMap: Map[ModuleId, List[SourcepathEntry]] =
      sourcepathEntries
        .groupBy(_.moduleId)
        .mapValues(xs => xs.map(x => SourcepathEntry(x.path, x.scope, x.managed)))
        .withDefaultValue(Nil)

    val classpathEntriesMap: Map[ModuleId, List[ClasspathEntry]] = {
      val internalModules = modules.map(x => (x.groupId, x.artifactId, x.version)).toSet

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

    val semanticdbs: List[s.TextDocument] =
      (projectPath  / AnalysisDirname / SemanticdbMergedFilename).inputStream
        .apply(input => s.TextDocument.streamFromDelimitedInput(input).toList)

    val subProjects: Seq[ModuleMetadata] = {
      val semanticdbMap: Map[ModuleId, List[s.TextDocument]] = {
        val sourcePaths2Module: List[(String, ModuleId)] =
          sourcepathEntries
            .sortBy(- _.path.length) // so the empty path in case sources are in root is at the end
            .map(x => x.path -> x.moduleId)

        val empty = Map[ModuleId, List[s.TextDocument]]().withDefaultValue(Nil)

        semanticdbs.foldLeft(empty) { (acc, sdb) =>
          sourcePaths2Module
            .collectFirst {
              case (p, moduleId) if sdb.uri.startsWith(p) => moduleId
            } match {
            case Some(moduleId) =>
              acc.updated(moduleId, sdb :: acc(moduleId))
            case None =>
              warnings += LoadingMetadataException(s"No module found for ${sdb.uri}")
              acc
          }
        }
      }

      val JavaLibraryExtensions = Set(".jar", ".zip")
      val classpathMap: Map[ModuleId, Classpath] =
        classpathEntriesMap
          .map {
            case (name, entries) =>
              val baseDir = projectPath.path
              val moduleClasspathCandidates =
                entries.map(_.path)
                  .distinct
                  .map(x => File(baseDir.resolve(x)))

              moduleClasspathCandidates
                .foreach { x =>
                  if (x.isDirectory && x.list.isEmpty) {
                    warnings += LoadingMetadataException(s"Classpath entry is an empty directory: $x")
                  }
                  if (!x.exists && (x.extension.contains("jar") || x.extension.contains("zip"))) {
                    warnings += LoadingMetadataException(s"Missing classpath file: $x")
                  }
                  if (x.hasExtension && !JavaLibraryExtensions.contains(x.extension.get)) {
                    warnings += LoadingMetadataException(s"Unsupported Java library extension in: $x")
                  }
                }

              val moduleClasspath =
                moduleClasspathCandidates
                  .filter(x => x.isDirectory || x.extension.exists(JavaLibraryExtensions.contains))

              val moduleOutput = modulesMap(name).classpath

              // this looks a bit counter intuitive since the moduleOutput should have already been
              // part of the moduleClasspath loaded from the metadata-dependencies.csv
              // this is true, but sometimes, test-classes are missing because they are only indirectly
              // generated by macros (e.g. twitter--scalding::com.twitter:scalding-serialization:0.18.0-SNAPSHOT:jvm)
              name -> (moduleOutput ++ Classpath(moduleClasspath.map(x => AbsolutePath(x.path))) ++ Libraries.JvmBootClasspath)
          }
          .withDefault(x => modulesMap(x).classpath ++ Libraries.JvmBootClasspath)

      for {
        module <- modules
        projectId = module.projectId
        moduleId = module.moduleId
        semanticdbs = semanticdbMap(moduleId) if semanticdbs.nonEmpty
        classpathEntries = classpathEntriesMap(moduleId)
        sourcepathEntries = sourcepathEntriesMap(moduleId)
        classpath = classpathMap(moduleId)
      } yield
        ModuleMetadata(
          projectPath.path,
          projectId + "::" + moduleId,
          module.groupId,
          module.artifactId,
          module.version,
          module.commit,
          module.scalaVersion,
          classpathEntries,
          sourcepathEntries,
          classpath,
          semanticdbs,
          module.outputClasspaths.toList,
          module.outputTestClasspaths.toList
        )
    }

    val projectId: String = modules.head.projectId

    val scalaVersion: String = modules.head.scalaVersion

    val sbtVersion: String = modules.head.sbtVersion

    (ProjectMetadata(projectPath.path, projectId, scalaVersion, sbtVersion, subProjects), warnings)
  }
}
