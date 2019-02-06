package cz.cvut.fit.prl.scala.implicits.model

import better.files._
import cats.Monoid

import cz.cvut.fit.prl.scala.implicits.metadata.MetadataFilenames.ExtractedImplicitsFilename

import com.typesafe.scalalogging.LazyLogging

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
) extends TypeResolver {

  override def resolveType(ref: DeclarationRef): Declaration = {
    modules(ref.moduleId).declarations(ref.declarationFqn)
  }
}

object Index extends LazyLogging {
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

  private def task[T](name: String, thunk: => T): T = {
    logger.debug(name + "...")

    val time = System.currentTimeMillis()

    try {
      val result = thunk
      val elapsed = System.currentTimeMillis() - time
      logger.debug(name + " in " + elapsed)
      result
    } catch {
      case e: Throwable =>
        logger.warn(name + " FAILED")
        throw e
    }
  }

  def apply(path: String): Index = apply(File(path))

  def apply(path: File): Index = {
    val input = if (path.isDirectory) {
      path / ExtractedImplicitsFilename
    } else {
      path
    }

    val index = task("Building index", {
      val projects = task(s"Loading implicits from $input", {
        input.inputStream.apply(Project.streamFromDelimitedInput(_).toList)
      })

      apply(projects)
    })

    logger.info(
      s"Loaded index from: $input (" +
        s"projects: ${index.projects.size}, " +
        s"modules: ${index.modules.size}, " +
        s"call sites: ${index.implicitCallSites.size}, " +
        s"declarations: ${index.implicitDeclarations.size}" +
        s")")

    index
  }

  def apply(projects: Iterable[Project]): Index = {
    task(s"Indexing", {
      val projectStream = projects.toStream
        .map(buildIndex)

      Monoid[Index].combineAll(projectStream)
    })
  }

  private def buildIndex(project: Project): Index =
    task(s"Index for project ${project.projectId}", {
      val moduleStream = project.modules.toStream
        .map(buildModuleIndex(project, _))

      Monoid[Index].combineAll(moduleStream)
    })

  private def buildModuleIndex(project: Project, module: Module): Index =
    task(s"Index for module ${module.moduleId}", {
      val implicitDeclarations = module.declarations.values.filter(_.isImplicit)

      Index(
        Map(project.projectId -> project),
        Map(module.moduleId -> module),
        implicitDeclarations,
        module.implicitCallSites,
        Map(project.projectId -> module.paths)
      )
    })
}
