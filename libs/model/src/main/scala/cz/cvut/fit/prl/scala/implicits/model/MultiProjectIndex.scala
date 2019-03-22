package cz.cvut.fit.prl.scala.implicits.model

import better.files._

import cats.Monoid
import cats.syntax.monoid._

import cz.cvut.fit.prl.scala.implicits.model.Util._

import com.typesafe.scalalogging.Logger

import org.slf4j.LoggerFactory


case class MultiProjectIndex(
                              projectMaps: Map[String, Project],
                              moduleMap: Map[String, Module]
                            ) extends Index {
  def implicitDeclarations: Iterable[Declaration] =
    for {
      p <- projects
      d <- p.implicitDeclarations
    } yield d

  def implicitCallSites: Iterable[CallSite] =
    for {
      p <- projects
      d <- p.implicitCallSites
    } yield d

  override def projects: Iterable[Project] = projectMaps.values
  override def modules: Iterable[Module] = moduleMap.values
  override def module(moduleId: String): Module = moduleMap(moduleId)
  override def project(projectId: String) : Project = projectMaps(projectId)
  override def resolveDeclaration(ref: DeclarationRef): Declaration = {
    moduleMap(ref.moduleId).declarations(ref.declarationId)
  }
}

object MultiProjectIndex {
  implicit val logger: Logger = Logger(LoggerFactory.getLogger(getClass.getName))
  implicit val monoid: Monoid[MultiProjectIndex] = new Monoid[MultiProjectIndex] {

    override def combine(x: MultiProjectIndex, y: MultiProjectIndex): MultiProjectIndex = {
      MultiProjectIndex(
        x.projectMaps ++ y.projectMaps,
        x.moduleMap ++ y.moduleMap,
      )
    }

    override def empty: MultiProjectIndex = MultiProjectIndex(Map[String, Project](), Map[String, Module]())
  }

  def fromFile(filename: String): MultiProjectIndex = fromFile(File(filename))

  def fromFile(file: File): MultiProjectIndex = {
    val index = timedTask(s"Loading implicits from $file") {
      file.inputStream.apply { input =>
        Project
          .streamFrom(input)
          .zipWithIndex
          .map(x => apply(x._1, x._2))
          .foldLeft(Monoid[MultiProjectIndex].empty)(_ |+| _)
      }
    }

    logger.info(
      s"Loaded index from: $file (" +
        s"projects: ${index.projectMaps.size}, " +
        s"modules: ${index.moduleMap.size}, " +
        s"call sites: ${index.implicitCallSites.size}, " +
        s"declarations: ${index.implicitDeclarations.size}" +
        s")"
    )

    index
  }

  def apply(projects: Traversable[Project]): MultiProjectIndex = {
    timedTask(s"Indexing") {
      val projectStream = projects.toStream
        .map(apply)

      Monoid[MultiProjectIndex].combineAll(projectStream)
    }
  }

  private def apply(project: Project): MultiProjectIndex = buildIndex(project, None)

  private def apply(project: Project, n: Int): MultiProjectIndex = buildIndex(project, Some(n))

  private def buildIndex(project: Project, n: Option[Int]): MultiProjectIndex = {
    val msg = "Indexing project " + n.getOrElse("") + " " + project.projectId

    timedTask(msg, timeIt = false) {
      val modules = project.modules.valuesIterator.map(buildModuleIndex(project, _))
      Monoid[MultiProjectIndex].combineAll(modules)
    }
  }

  private def buildModuleIndex(project: Project, module: Module): MultiProjectIndex =
    MultiProjectIndex(
      Map(project.projectId -> project),
      Map(module.moduleId -> module)
    )
}
