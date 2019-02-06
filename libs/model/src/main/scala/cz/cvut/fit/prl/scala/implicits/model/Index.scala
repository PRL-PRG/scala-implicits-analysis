package cz.cvut.fit.prl.scala.implicits.model

import better.files._
import cats.Monoid

import com.typesafe.scalalogging.LazyLogging

/**
  * @param projects - a map from projects' ids to projects
  * @param modules - a map from modules' ids to modules
  * @param implicitDeclarations - a list of implicit declarations
  * @param implicitCallSites - a list of implicit call sites
  * @param locationsModules - a map from Locations identity hashCodes to a modules
  */
case class Index(
    projects: Map[String, Project],
    modules: Map[String, Module],
    implicitDeclarations: Iterable[Declaration],
    implicitCallSites: Iterable[CallSite],
    locationsModules: Map[Int, Module]
) extends TypeResolver {

  def project(module: Module): Project = projects(module.projectId)

  def project(location: Location): Project = module(location).project(this)

  def module(location: Location): Module = locationsModules(location.identityHashCode)

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
        x.locationsModules ++ y.locationsModules
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

  def apply(path: File): Index =
    task("Building index", {
      val projects = task(s"Loading implicits from $path", {
        path.inputStream.apply(Project.streamFromDelimitedInput(_).toList)
      })

      apply(projects)
    })

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
      val empty = (List[Declaration](), Map[Int, Module]())

      val (implicitDeclarations, declarationsLocations) =
        module.declarations.values.foldLeft(empty) {
          case ((implicits, locations), declaration) =>
            (
              if (declaration.isImplicit) declaration :: implicits else implicits,
              locations + (declaration.location.identityHashCode -> module)
            )
        }

      val csLocations =
        module.implicitCallSites
          .foldLeft(Map[Int, Module]())((locations, cs) =>
      locations + (cs.location.identityHashCode -> module))

      Index(
        Map(project.projectId -> project),
        Map(module.moduleId -> module),
        implicitDeclarations,
        module.implicitCallSites,
        declarationsLocations ++ csLocations
      )
    })
}
