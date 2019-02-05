package cz.cvut.fit.prl.scala.implicits.model

import better.files._
import cats.Monoid
import cats.syntax.monoid._
import com.typesafe.scalalogging.LazyLogging

/**
    *
    * @param projects
    * @param modulesProjects
    * @param implicitDeclarations
    * @param implicitCallSites
    * @param declarationsModules - a map from Declaration identity hashCode to a Module
    * @param callSitesModules - a map from CallSite identity hashCode to a Module
    * @param locationsModules - a map from Location identity hashCode to a Module
    */
case class Index(
    projects: Map[String, Project],
    modules: Map[String, Module],
    implicitDeclarations: List[Declaration],
    implicitCallSites: List[CallSite],
    locationsModules: Map[Int, Module]
) extends TypeResolver {
    def project(module: Module): Project = projects(module.projectId)
  //    def project(declaration: Declaration): Project = declaration.
//    def project(callSite: CallSite): Project = modulesProjects(module(callSite))
  def project(location: Location): Project = module(location).project(this)
  //    def module(declaration: Declaration): Module = declarationsModules(declaration.identityHashCode)
//    def module(callSite: CallSite): Module = callSitesModules(callSite.identityHashCode)
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

      task(s"Indexing", {
        projects.toStream.map(buildIndex).reduce(_ |+| _)
      })
    })

  private def buildIndex(project: Project): Index =
    task(s"Index for project ${project.projectId}", {
      project.modules.toStream
        .map(buildModuleIndex(project, _))
        .foldLeft(Index.monoid.empty)(_ |+| _)
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

      val (implicitCallSites, callSitesLocations) = {
        val empty = (List[CallSite](), Map[Int, Module]())

        project.implicitCallSites
          .foldLeft(empty) {
            case ((callSites, locations), callSite) =>
              (
                callSite :: callSites,
                callSite.location
                  .map(x => locations + (x.identityHashCode -> module))
                  .getOrElse(locations)
              )
          }
      }

      Index(
        Map(project.projectId -> project),
        Map(module.moduleId -> module),
        implicitDeclarations,
        implicitCallSites,
        declarationsLocations ++ callSitesLocations
      )
    })
}
