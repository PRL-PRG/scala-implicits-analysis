package cz.cvut.fit.prl.scala.implicits.model

import better.files._
import cats.Monoid
import cz.cvut.fit.prl.scala.implicits.metadata.MetadataFilenames._
import cz.cvut.fit.prl.scala.implicits.model.Util.timedTask
import com.typesafe.scalalogging.{LazyLogging, Logger}
import org.slf4j.LoggerFactory

import scala.util.Try

/**
  * @param projects - a map from projects' ids to projects
  * @param modules - a map from modules' ids to modules
  * @param implicitDeclarations - a list of all implicit declarations
  * @param implicitCallSites - a list of all implicit call sites
  */
case class Index(
    projects: Map[String, Project],
    modules: Map[String, Module],
    implicitDeclarations: Iterable[Declaration],
    implicitCallSites: Iterable[CallSite],
    paths: Map[String, Map[String, PathEntry]]
) extends DeclarationResolver {

  override def resolveDeclaration(ref: DeclarationRef): Declaration = {
    modules(ref.moduleId).declarations(ref.declarationId)
  }
}

object Index {
  implicit val logger = Logger(LoggerFactory.getLogger(getClass.getName))
  implicit val monoid: Monoid[Index] = new Monoid[Index] {

    override def combine(x: Index, y: Index): Index = {
      Index(
        x.projects ++ y.projects,
        x.modules ++ y.modules,
        x.implicitDeclarations ++ y.implicitDeclarations,
        x.implicitCallSites ++ y.implicitCallSites,
        y.paths.foldLeft(x.paths) {
          case (prev, (k, v)) =>
            prev.updated(k, prev.get(k).map(_ ++ v).getOrElse(v))
        }
      )
    }

    override def empty: Index = Index(Map(), Map(), List(), List(), Map())
  }

  def fromProjectsFile(path: File): Index = {
    if (!path.isDirectory) {
      throw new IllegalArgumentException(s"$path must be a directory")
    }

    val index = timedTask("Building index", {
      val indexes =
        for {
          projectName <- (File.currentWorkingDirectory / ProjectsFilename).lineIterator
          projectPath = path / ProjectsDirname / projectName
          dataFile = projectPath / AnalysisDirname / ExtractedImplicitsFilename if dataFile.exists
        } yield
          fromDataFile(dataFile)

      Monoid[Index].combineAll(indexes)
    })

    logger.info(
      s"Loaded index from: $path (" +
        s"projects: ${index.projects.size}, " +
        s"modules: ${index.modules.size}, " +
        s"call sites: ${index.implicitCallSites.size}, " +
        s"declarations: ${index.implicitDeclarations.size}" +
        s")")

    index
  }

  def fromDataFile(dataFile: File): Index = {
    val project = timedTask(s"Loading implicits from $dataFile", {
      dataFile.inputStream.apply(Project.parseFrom)
    })

    apply(project)
  }

  def apply(project: Project): Index = buildIndex(project)

  def apply(projects: Traversable[Project]): Index = {
    timedTask(s"Indexing", {
      val projectStream = projects.toStream
        .map(apply)

      Monoid[Index].combineAll(projectStream)
    })
  }

  private def buildIndex(project: Project): Index =
    timedTask(s"Index for project ${project.projectId}", {
      val moduleStream = project.modules.toStream
        .map(buildModuleIndex(project, _))

      Monoid[Index].combineAll(moduleStream)
    })

  private def buildModuleIndex(project: Project, module: Module): Index =
    timedTask(s"Index for module ${module.moduleId}", {
      val implicitDeclarations =
        module.declarations.values.filter(x => x.isImplicit || x.hasImplicitParameters)

      Index(
        Map(project.projectId -> project),
        Map(module.moduleId -> module),
        implicitDeclarations,
        module.implicitCallSites,
        Map(project.projectId -> module.paths)
      )
    })
}
