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

import scala.annotation.tailrec
import scala.collection.concurrent.TrieMap
import scala.collection.mutable
import scala.meta._
import scala.meta.internal.{semanticdb => s}
import scala.meta.io.{AbsolutePath, Classpath}

case class SubProjectMetadata(
    moduleId: String,
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

    def readCSV[T: HeaderDecoder](file: File): List[T] = {
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
        case vs =>
          // s rough heuristic
          warnings += new Exception(
            s"There are multiple projects under the same name -- taking head: $vs")
          vs.head
      }

    val sourcepathEntriesMap =
      sourcepathEntries
        .groupBy(_.moduleId)
        .mapValues(xs => xs.map(x => SourcepathEntry(x.path, x.scope, x.managed)))
        .withDefaultValue(Nil)

    val internalClasspathEntriesMap: Map[String, List[ClasspathEntry]] = {
      val dependencyMap: Map[String, List[String]] = internalDependencies
        .map { x =>
          val module = x.moduleId
          val platform = versionsMap(module).platform
          val candidates = Seq(
            x.dependencyId(platform),
            x.dependencyId("jvm")
          )

          // we need either the dependency for the platform version of the module or JVM version
          val dependencyId = candidates.collectFirst {
            case x if versionsMap.get(x).isDefined => x
          }

          module -> dependencyId.getOrThrow {
            val e =
              new Exception(s"Unable to find dependency ${x.dependencyId("")}, tried $candidates")
            e
          }
        }
        .groupBy(_._1)
        .mapValues(_.map(_._2))
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
                      classpath,
                      version.groupId,
                      version.artifactId,
                      version.version,
                      scope,
                      internal = true,
                      managed = true
                    )
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
              dependency.path,
              dependency.groupId,
              dependency.artifactId,
              dependency.version,
              dependency.scope,
              internal = false,
              managed = true
            )
          }
        }

    // we need to compose the maps, not just concatenate
    val classpathEntriesMap =
      (internalClasspathEntriesMap |+| externalClasspathEntriesMap).withDefaultValue(Nil)

    val semanticdbs: List[Semanticdb] =
      (path / Constants.MergedSemanticdbFilename).inputStream
        .apply(input => Semanticdb.streamFromDelimitedInput(input).toList)

    val subProjects: Seq[SubProjectMetadata] = {
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

              val moduleOutput = versionsMap
                .get(name)
                .map(m => m.output.map(AbsolutePath(_)))
                .getOrElse(Nil)
                .toList

              name -> (Classpath(moduleOutput) ++ Classpath(moduleClasspath) ++ Libraries.JvmBootClasspath)
          }
          .withDefaultValue(Libraries.JvmBootClasspath)

      for {
        version <- versions
        moduleId = version.moduleId
        semanticdbs = semanticdbMap(moduleId) if semanticdbs.nonEmpty
        classpathEntries = classpathEntriesMap(moduleId)
        sourcepathEntries = sourcepathEntriesMap(moduleId)
        classpath = classpathMap(moduleId)
      } yield
        SubProjectMetadata(moduleId, classpathEntries, sourcepathEntries, classpath, semanticdbs)
    }

    val projectId: String = versions.head.projectId

    val scalaVersion: String = versions.head.scalaVersion

    val sbtVersion: String = versions.head.sbtVersion

    (ProjectMetadata(path, projectId, scalaVersion, sbtVersion, subProjects), warnings)
  }
}
