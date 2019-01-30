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
    projects: List[Project],
    modulesProjects: Map[Module, Project],
    implicitDeclarations: Map[String, Declaration],
    implicitCallSites: List[CallSite],
    declarationsModules: Map[Int, Module],
    callSitesModules: Map[Int, Module],
    locationsModules: Map[Int, Module]
) extends TypeResolver {
    def project(module: Module): Project = modulesProjects(module)
    def project(declaration: Declaration): Project = modulesProjects(module(declaration))
    def project(callSite: CallSite): Project = modulesProjects(module(callSite))
    def project(location: Location): Project = modulesProjects(module(location))
    def module(declaration: Declaration): Module = declarationsModules(declaration.identityHashCode)
    def module(callSite: CallSite): Module = callSitesModules(callSite.identityHashCode)
    def module(location: Location): Module = locationsModules(location.identityHashCode)

    override def resolveType(tpe: Type): Declaration = {
      implicitDeclarations(tpe.declarationRef)
    }
  }

object Index extends LazyLogging {
  implicit val monoid: Monoid[Index] = new Monoid[Index] {
    override def combine(x: Index, y: Index) = {
      Index(
        x.projects ++ y.projects,
        x.modulesProjects ++ y.modulesProjects,
        x.implicitDeclarations ++ y.implicitDeclarations,
        x.implicitCallSites ++ y.implicitCallSites,
        x.declarationsModules ++ y.declarationsModules,
        x.callSitesModules ++ y.callSitesModules,
        x.locationsModules ++ y.locationsModules
      )
    }
    override def empty: Index = Index(List(), Map(), Map(), List(), Map(), Map(), Map())
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
      val empty = (Map[Int, Module](), Map[String, Declaration](), Map[Int, Module]())

      val (declarationMap, implicitDeclarations, declarationsLocations) =
        module.declarations.foldLeft(empty) {
          case ((map, implicits, locations), declaration) =>
            (
              map + (declaration.identityHashCode -> module),
              if (declaration.isImplicit) implicits + (declaration.fqn -> declaration) else implicits,
              declaration.location
                .map(x => locations + (x.identityHashCode -> module))
                .getOrElse(locations)
            )
        }

      val (callSiteMap, implicitCallSites, callSitesLocations) = {
        val empty = (Map[Int, Module](), List[CallSite](), Map[Int, Module]())

        project.implicitCallSites
          .foldLeft(empty) {
            case ((map, callSites, locations), callSite) =>
              (
                map + (callSite.identityHashCode -> module),
                callSite :: callSites,
                callSite.location
                  .map(x => locations + (x.identityHashCode -> module))
                  .getOrElse(locations)
              )
          }
      }

      Index(
        List(project),
        Map(module -> project),
        implicitDeclarations,
        implicitCallSites,
        declarationMap,
        callSiteMap,
        declarationsLocations ++ callSitesLocations
      )
    })
}
