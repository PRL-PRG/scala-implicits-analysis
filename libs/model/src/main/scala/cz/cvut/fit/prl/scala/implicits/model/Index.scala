package cz.cvut.fit.prl.scala.implicits.model

import better.files._
import cats.Semigroup
import cats.syntax.semigroup._

case class Index(
    projects: List[Project],
    moduleMap: Map[Module, Project],
    implicitDeclarations: List[Declaration],
    implicitCallSites: List[CallSite],
    declarationMap: Map[Int, Module],
    callSiteMap: Map[Int, Module],
    locationMap: Map[Int, Module]
)

case class Library(groupId: String, artifactId: String, version: String)

object Index {
  implicit val semigroup = new Semigroup[Index] {
    override def combine(x: Index, y: Index) = {
      Index(
        x.projects ++ y.projects,
        x.moduleMap ++ y.moduleMap,
        x.implicitDeclarations ++ y.implicitDeclarations,
        x.implicitCallSites ++ y.implicitCallSites,
        x.declarationMap ++ y.declarationMap,
        x.callSiteMap ++ y.callSiteMap,
        x.locationMap ++ y.locationMap
      )
    }
  }

  implicit class IdentityHashCode(x: AnyRef) {
    def identityHashCode: Int = System.identityHashCode(x)
  }

  implicit class ModuleOps(that: Module)(implicit idx: Index) {
    def project: Project = idx.moduleMap(that)
  }

//  trait LocationOps {
//    def location: Location
//    def inModule(implicit idx: Index): Boolean =
//
//  }

  implicit class DeclarationOps(that: Declaration)(implicit idx: Index) {
    //def inModule: Boolean = that.location.map(location => idx.declarationMap(that.identityHashCode) == idx.locationMap(location))
  }

  //  implicit class LocationOps(that: Location)(implicit idx: Index) {
  //    def githubURL: String = ???
  //    def isLocal: Boolean = ???
  //    def isJDK: Boolean = ???
  //    def library: Option[Library] = idx.libraries.get(that)
  //    def project: Option[Project] = idx.locations.get(System.identityHashCode(that))
  //  }

  def apply(path: File): Index = {
    val projects = path.inputStream.apply(Project.streamFromDelimitedInput(_).toList)

    def buildModuleIndex(project: Project, module: Module): Index = {
      val empty = (Map[Int, Module](), List[Declaration](), Map[Int, Module]())

      val (declarationMap, implicitDeclarations, declarationsLocations) =
        module.declarations.foldLeft(empty) {
          case ((map, implicits, locations), declaration) =>
            (
              map + (declaration.identityHashCode -> module),
              if (declaration.isImplicit) declaration :: implicits else implicits,
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
    }

    def buildIndex(project: Project): Index = {
      project.modules.toStream.map(buildModuleIndex(project, _)).reduce(_ |+| _)
    }

    projects.toStream.map(buildIndex).reduce(_ |+| _)
  }
}
