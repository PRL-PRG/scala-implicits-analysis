package cz.cvut.fit.prl.scala.implicits.model

import java.io.InputStream

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
trait Index extends DeclarationResolver {
  def projects: Map[String, Project]

  def modules: Map[String, Module]

  def paths: Map[String, Map[String, PathEntry]]

  def implicitDeclarations: Iterable[Declaration]

  def implicitCallSites: Iterable[CallSite]

  override def resolveDeclaration(ref: DeclarationRef): Declaration = {
    modules(ref.moduleId).declarations(ref.declarationId)
  }
}

class StreamingIndex(dataFile: File) extends Index {
  private var currentIndex: Index = _

  class ProjectIterator[A](open: Index => Iterator[A]) extends Iterator[A] {
    lazy val input: InputStream = dataFile.newInputStream
    lazy val projectIterator: Iterator[Project] = Project.streamFrom(input).iterator

    var currentIterator: Iterator[A] = Seq.empty.iterator

    override def hasNext: Boolean = {
      if (currentIterator.hasNext) {
        true
      } else if (projectIterator.hasNext) {
        currentIndex = FullIndex(projectIterator.next())
        currentIterator = open(currentIndex)
        hasNext
      } else {
        input.close()
        false
      }
    }

    override def next(): A = currentIterator.next()
  }

  override def projects: Map[String, Project] = currentIndex.projects
  override def modules: Map[String, Module] = currentIndex.modules
  override def paths: Map[String, Map[String, PathEntry]] = currentIndex.paths

  override def implicitDeclarations: Iterable[Declaration] = new Iterable[Declaration] {
    def iterator = new ProjectIterator[Declaration](_.implicitDeclarations.iterator)
  }

  override def implicitCallSites: Iterable[CallSite] = new Iterable[CallSite] {
    override def iterator: Iterator[CallSite] = {
      val input = dataFile.newInputStream
      val projectIterator = Project.streamFrom(input).iterator
      var currentIterator: Iterator[CallSite] = Seq.empty.iterator

      new Iterator[CallSite] {
        override def hasNext: Boolean = {
          if (currentIterator.hasNext) {
            true
          } else if (projectIterator.hasNext) {
            currentIndex = FullIndex(projectIterator.next())
            currentIterator = currentIndex.implicitCallSites.iterator
            hasNext
          } else {
            input.close()
            false
          }
        }

        override def next(): CallSite = currentIterator.next()
      }
    }
  }
}

case class FullIndex(
    projects: Map[String, Project],
    modules: Map[String, Module],
    paths: Map[String, Map[String, PathEntry]]
) extends Index {
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
}

object FullIndex {
  type FailedProject = (String, Throwable)

  implicit val logger: Logger = Logger(LoggerFactory.getLogger(getClass.getName))
  implicit val monoid: Monoid[FullIndex] = new Monoid[FullIndex] {

    override def combine(x: FullIndex, y: FullIndex): FullIndex = {
      FullIndex(
        x.projects ++ y.projects,
        x.modules ++ y.modules,
        y.paths.foldLeft(x.paths) {
          case (prev, (k, v)) =>
            prev.updated(k, prev.get(k).map(_ ++ v).getOrElse(v))
        }
      )
    }

    override def empty: FullIndex = FullIndex(Map(), Map(), Map())
  }

  def fromFile(filename: String): FullIndex = fromFile(File(filename))

  def fromFile(file: File): FullIndex = {
    val index = timedTask(s"Loading implicits from $file") {
      file.inputStream.apply { input =>
        Project
          .streamFrom(input)
          .zipWithIndex
          .map(x => apply(x._1, x._2))
          .foldLeft(Monoid[FullIndex].empty)(_ |+| _)
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

  def apply(project: Project): FullIndex = buildIndex(project, None)

  def apply(project: Project, n: Int): FullIndex = buildIndex(project, Some(n))

  def apply(projects: Traversable[Project]): FullIndex = {
    timedTask(s"Indexing") {
      val projectStream = projects.toStream
        .map(apply)

      Monoid[FullIndex].combineAll(projectStream)
    }
  }

  private def buildIndex(project: Project, n: Option[Int]): FullIndex = {
    val msg = "Indexing project " + n.getOrElse("") + " " + project.projectId

    timedTask(msg, false) {
      val moduleStream = project.modules.toStream
        .map(buildModuleIndex(project, _))

      Monoid[FullIndex].combineAll(moduleStream)
    }
  }

  private def buildModuleIndex(project: Project, module: Module): FullIndex =
    try {
      FullIndex(
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
