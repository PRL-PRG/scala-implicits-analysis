package cz.cvut.fit.prl.scala.implicits.model

import better.files._
import cats.Semigroup
import cats.syntax.semigroup._

case class Index(
    projects: List[Project],
    implicitDeclarations: List[Declaration],
    declarationMap: Map[Declaration, Project],
    implicitCallSiteMap: Map[CallSite, Project],
    locationMap: Map[Int, Project]
)

case class Library(groupId: String, artifactId: String, version: String)

object Index {

  implicit val semigroup = new Semigroup[Index] {
    override def combine(x: Index, y: Index) = {
      Index(
        x.projects ++ y.projects,
        x.implicitDeclarations ++ y.implicitDeclarations,
        x.declarationMap ++ y.declarationMap,
        x.implicitCallSiteMap ++ y.implicitCallSiteMap,
        x.locationMap ++ y.locationMap
      )
    }
}

  implicit class IdentityHashCode(x: AnyRef) {
    def identityHashCode: Int = System.identityHashCode(x)
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

    def buildIndex(project: Project): Index = {
      val (declarations, implicitDeclarations, declLocations) = {
        val empty = (Map[Declaration, Project](), List[Declaration](), Map[Int, Project]())

        project.declarations
          .foldLeft(empty) {
            case ((map, impls, locations), decl) =>
              (
                map + (decl -> project),
                if (decl.isImplicit) decl :: impls else impls,
                decl.location.map(x => locations + (x.identityHashCode -> project)).getOrElse(locations)
              )
          }
      }

      val (callSites, csLocations) = {
        val empty = (Map[CallSite, Project](), Map[Int, Project]())

        project.implicitCallSites
          .foldLeft(empty) {
            case ((map, locations), cs) =>
              (
                map + (cs -> project),
                cs.location.map(x => locations + (x.identityHashCode -> project)).getOrElse(locations)
              )
          }
      }

      Index(
        List(project),
        implicitDeclarations,
        declarations,
        callSites,
        declLocations ++ csLocations
      )
    }

    projects.toStream.map(buildIndex).reduce((a, b) => a |+| b)
  }

  def parseLibrary(location: Location): Option[Library] = {
    val tries: Stream[Option[Library]] = Stream(
      parseIvy(location.uri),
      parseCoursier(location.uri)
    )

    tries.collectFirst { case Some(library) => library }
  }

  // /usr/lib/jvm/java-8-openjdk/jre/lib/rt.jar!java/util/List.class
  // /home/krikava/.cache/coursier/v1/https/repo1.maven.org/maven2/com/chuusai/shapeless_2.12/2.3.3/shapeless_2.12-2.3.3.jar!shapeless/Strict.class
  // /home/krikava/Research/Projects/scala-corpus/corpora/1-single/projects/ensime--ensime-server/.sbt-boot/scala-2.12.4/lib/scala-library.jar!scala/Function1.class
  def parseIvy(str: String): Option[Library] = {
    None
  }
  def parseCoursier(str: String): Option[Library] = {
    None
  }
}
