package cz.cvut.fit.prl.scala.implicits.model

import better.files._
import cats.Monoid
import cats.syntax.monoid._
import cz.cvut.fit.prl.scala.implicits.model.Util._
import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

/**
  * @param projects - a map from projects' ids to projects
  * @param modules - a map from modules' ids to modules
  */
case class Index(
    projects: Map[String, Project],
    modules: Map[String, Module],
    paths: Map[String, Map[String, PathEntry]]
) extends DeclarationResolver {
  def implicitDeclarations: Iterable[Declaration] =
    for {
      p <- projects.values
      d <- p.implicitDeclarations
    } yield d

  def implicitCallSites: Iterable[CallSite] =
    for {
      p <- projects.values
      d <- p.implicitCallSites
    } yield d

  override def resolveDeclaration(ref: DeclarationRef): Declaration = {
    modules(ref.moduleId).declarations(ref.declarationId)
  }
}

object Index {
  type FailedProject = (String, Throwable)

  implicit val logger: Logger = Logger(LoggerFactory.getLogger(getClass.getName))
  implicit val monoid: Monoid[Index] = new Monoid[Index] {

    override def combine(x: Index, y: Index): Index = {
      Index(
        x.projects ++ y.projects,
        x.modules ++ y.modules,
        y.paths.foldLeft(x.paths) {
          case (prev, (k, v)) =>
            prev.updated(k, prev.get(k).map(_ ++ v).getOrElse(v))
        }
      )
    }

    override def empty: Index = Index(Map(), Map(), Map())
  }

  def fromFile(filename: String): Index = fromFile(File(filename))

  def fromFile(file: File): Index = {
    val index = timedTask(s"Loading implicits from $file") {
      file.inputStream.apply { input =>
        Project
          .streamFrom(input)
          .zipWithIndex
          .map(x => apply(x._1, x._2))
          .foldLeft(Monoid[Index].empty)(_ |+| _)
      }
    }

    logger.info(
      s"Loaded index from: $file (" +
        s"projects: ${index.projects.size}, " +
        s"modules: ${index.modules.size}, " +
        s"call sites: ${index.implicitCallSites.size}, " +
        s"declarations: ${index.implicitDeclarations.size}" +
        s")"
    )

    index
  }

  def apply(project: Project): Index = buildIndex(project, None)

  def apply(project: Project, n: Int): Index = buildIndex(project, Some(n))

  def apply(projects: Traversable[Project]): Index = {
    timedTask(s"Indexing") {
      val projectStream = projects
        .toStream
        .map(apply)

      Monoid[Index].combineAll(projectStream)
    }
  }

  private def buildIndex(project: Project, n: Option[Int]): Index = {
    val msg = "Indexing project " + n.getOrElse("") + " " + project.projectId

    timedTask(msg, false) {
      val moduleStream = project.modules.toStream
        .map(buildModuleIndex(project, _))

      Monoid[Index].combineAll(moduleStream)
    }
  }

  private def buildModuleIndex(project: Project, module: Module): Index =
    try {
      Index(
        Map(project.projectId -> project),
        Map(module.moduleId -> module),
        Map(project.projectId -> module.paths)
      )
    } catch {
      case e: Throwable =>
        throw new Exception(
          s"Unable to build modules index for ${project.projectId} -- ${module.moduleId}",
          e)
    }
}
