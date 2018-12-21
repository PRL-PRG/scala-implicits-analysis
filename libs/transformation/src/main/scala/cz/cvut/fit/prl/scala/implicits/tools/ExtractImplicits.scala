package cz.cvut.fit.prl.scala.implicits.tools

import better.files._
import cats.Monoid
import cats.implicits._
import cats.derived
import cz.cvut.fit.prl.scala.implicits.Constants._
import cz.cvut.fit.prl.scala.implicits.{ProjectMetadata, SubProjectMetadata}
import cz.cvut.fit.prl.scala.implicits.extractor.{
  CallSiteExtractor,
  DeclarationExtractor,
  ExtractionContext
}
import cz.cvut.fit.prl.scala.implicits.model.{CallSite, Declaration, PathEntry, Project}
import cz.cvut.fit.prl.scala.implicits.utils._
import kantan.csv._
import kantan.csv.ops._
import kantan.csv.generic._

import scala.util.{Failure, Success, Try}

object ExtractImplicits extends App {
  case class Stats(declarations: Int, callSites: Int, failures: Int) {
    override def toString: String = s"$declarations, $callSites, $failures"
  }

  object Stats {
    implicit val monoid: Monoid[Stats] = {
      import derived.auto.monoid._
      derived.semi.monoid
    }
  }

  case class Result(project: Project, exceptions: Seq[(String, Throwable)]) {
    def stats = Stats(project.declarations.size, project.implicitCallSites.size, exceptions.size)
  }

  class OutputWriter(messageFile: File, exceptionFile: File, statusFile: File) {

    sealed trait Status {
      def projectId: String
      def failure: String
      def stats: Stats
    }

    object Status {
      implicit val statusEncoder = RowEncoder.encoder(0, 1, 2, 3, 4)((x: Status) =>
        (x.projectId, x.failure, x.stats.declarations, x.stats.callSites, x.stats.failures))
    }

    case class SuccessfulExtraction(projectId: String, stats: Stats) extends Status {
      override def failure: String = "NA"
    }

    case class FailedExtraction(projectId: String, exception: Throwable) extends Status {
      override def failure: String = s"${exception.getClass.getSimpleName}: ${exception.getMessage}"
      override def stats: Stats = Monoid[Stats].empty
    }

    case class ExtractionException(
        projectId: String,
        moduleId: String,
        cause: String,
        message: String,
        trace: String)

    object ExtractionException {
      def apply(projectId: String, moduleId: String, exception: Throwable): ExtractionException = {
        val cause = Option(exception.getCause).getOrElse(exception)
        val trace = cause.getStackTrace.head.toString

        ExtractionException(
          projectId,
          moduleId,
          cause.getClass.getSimpleName,
          cause.getMessage,
          trace)
      }
    }

    private val messageOutput = messageFile.newOutputStream
    private val exceptionOutput =
      exceptionFile.newOutputStream
        .asCsvWriter[ExtractionException](
          rfc.withHeader("project_id", "module_id", "cause", "message", "trace"))

    private val statusOutput =
      statusFile.newOutputStream
        .asCsvWriter[Status](
          rfc.withHeader("project_id", "failure", "declarations", "callsites", "failures"))

    def write(projectId: String, result: Try[Result]): Stats = {
      val status = result match {
        case Success(value) => SuccessfulExtraction(projectId, process(projectId, value))
        case Failure(exception) => FailedExtraction(projectId, exception)
      }

      statusOutput.write(status)
      status.stats
    }

    def process(projectId: String, result: Result): Stats = {
      result.project.writeDelimitedTo(messageOutput)
      result.exceptions
        .map {
          case (moduleId, e) => ExtractionException(projectId, moduleId, e)
        }
        .foreach(exceptionOutput.write)

      result.stats
    }

    def close(): Unit = {
      Seq(messageOutput, exceptionOutput, statusOutput)
        .map(x => Try(x.close()))
        .foreach(_.get)
    }
  }

  case class SubProjectResult(
      declarations: List[Declaration],
      implicitCallSites: List[CallSite],
      allCallSitesCount: Int,
      paths: List[PathEntry],
      exceptions: List[(String, Throwable)]
  )

  object SubProjectResult {
    implicit val monoid: Monoid[SubProjectResult] = {
      import derived.auto.monoid._
      derived.semi.monoid
    }
  }

  def extractProject(projectPath: File): Result = {

    val (metadata, warnings) = ProjectMetadata(projectPath)
    val subProjectResult =
      metadata.subProjects
        .foldLeft(Monoid[SubProjectResult].empty)((a, b) => a |+| extractSubProject(b))

    val project = Project(
      metadata.projectId,
      metadata.scalaVersion,
      metadata.sbtVersion,
      subProjectResult.paths.map(x => x.path -> x).toMap,
      subProjectResult.declarations,
      subProjectResult.implicitCallSites,
      subProjectResult.allCallSitesCount
    )

    Result(
      project,
      subProjectResult.exceptions ++ warnings.map(x => "ROOT" -> x)
    )
  }

  def extractSubProject(metadata: SubProjectMetadata): SubProjectResult = {
    val ctx = new ExtractionContext(metadata.resolver)
    val declExtractor = new DeclarationExtractor(ctx)
    val csExtractor = new CallSiteExtractor(ctx)

    val (_, declExceptions) =
      metadata.semanticdbs
        .flatMap(declExtractor.extractImplicitDeclarations)
        .split()

    val (callSites, csExceptions) =
      metadata.semanticdbs
        .map(x => x -> metadata.ast(x.uri))
        .flatMap { case (db, ast) => csExtractor.extractImplicitCallSites(db, ast) }
        .split()

    val csCount =
      metadata.semanticdbs
        .map(x => metadata.ast(x.uri))
        .map(csExtractor.callSiteCount)
        .sum

    val classpath = metadata.classpathEntries ++ Libraries.JvmBootModelClasspath
    val exceptions = (declExceptions ++ csExceptions).map(x => metadata.moduleId -> x)

    SubProjectResult(
      declarations = ctx.declarations,
      implicitCallSites = callSites,
      allCallSitesCount = csCount,
      paths = classpath ++ metadata.sourcepathEntries,
      exceptions = exceptions
    )
  }

  def run(projectPath: File, outputPath: File): Unit = {
    val messageFile = outputPath / ExtractedImplicitsFilename
    val exceptionFile = outputPath / ExtractionExceptionsFilename
    val statusFile = outputPath / ExtractionStatusFilename

    val outputWriter = new OutputWriter(messageFile, exceptionFile, statusFile)

    val result = Try(extractProject(projectPath))

    outputWriter.write(projectPath.name, result)
    outputWriter.close()

    result match {
      case Success(value) => println(value.stats)
      case Failure(exception) => exception.printStackTrace()
    }
  }

  args.toList match {
    case projectPath :: outputPath :: Nil => run(File(projectPath), File(outputPath))
    case _ => sys.error("Usage: <source> <output>")
  }
}
