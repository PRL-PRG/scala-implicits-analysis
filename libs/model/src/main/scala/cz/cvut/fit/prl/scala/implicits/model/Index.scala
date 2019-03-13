package cz.cvut.fit.prl.scala.implicits.model

import better.files._
import cats.Monoid
import cats.syntax.monoid._
import cz.cvut.fit.prl.scala.implicits.model.Util.timedTask
import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

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
  type FailedProject = (String, Throwable)

  implicit val logger: Logger = Logger(LoggerFactory.getLogger(getClass.getName))
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

  def saveToProjectsFile(index: Index, filename: String): Unit =
    saveToProjectsFile(index, File(filename))

  def saveToProjectsFile(index: Index, file: File): Unit = {
    file.outputStream.apply { output =>
      index.projects.values.foreach(_.writeDelimitedTo(output))
    }
  }

  def fromProjectsFile(filename: String): Index = fromProjectsFile(File(filename))

  def fromProjectsFile(file: File): Index = {
    val index = timedTask(s"Loading implicits from $file", {
      file.inputStream.apply { input =>
        Project
          .streamFromDelimitedInput(input)
          .map(apply)
          .foldLeft(Monoid[Index].empty)(_ |+| _)
      }
    })

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

  def fromProjectFile(file: File): Index = {
    val project = timedTask(s"Loading implicits from $file", {
      file.inputStream.apply(Project.parseFrom)
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
    timedTask(s"Indexing project ${project.projectId}", {
      val moduleStream = project.modules.toStream
        .map(buildModuleIndex(project, _))

      Monoid[Index].combineAll(moduleStream)
    })

  private def buildModuleIndex(project: Project, module: Module): Index =
    timedTask(s"Indexing module ${module.moduleId}", {
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
