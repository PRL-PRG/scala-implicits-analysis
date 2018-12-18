package cz.cvut.fit.prl.scala.implicits.tools

import better.files._
import cats.Monoid
import cats.implicits._
import cats.derived
import cz.cvut.fit.prl.scala.implicits.Constants._
import cz.cvut.fit.prl.scala.implicits.{Constants, ProjectMetadata, SubProjectMetadata}
import cz.cvut.fit.prl.scala.implicits.extractor.{CallSiteExtractor, DeclarationExtractor, ExtractionContext}
import cz.cvut.fit.prl.scala.implicits.model.{CallSite, Declaration, PathEntry, Project}
import cz.cvut.fit.prl.scala.implicits.utils._
import kantan.csv._
import kantan.csv.ops._
import kantan.csv.generic._

import scala.util.{Failure, Success, Try}

object ExtractImplicits extends App {

  case class Result(project: Project, exceptions: Seq[Throwable])
  case class Stats(declarations: Int, callSites: Int, failures: Int) {
    override def toString: String = s"$declarations, $callSites, $failures"
  }

  object Stats {
    implicit val monoid: Monoid[Stats] = {
      import derived.auto.monoid._
      derived.semi.monoid
    }
  }

  class OutputWriter(messageFile: File, exceptionFile: File, statusFile: File)
      extends ((String, Try[Result]) => Stats) {

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

    case class ExtractionException(projectId: String, cause: String, message: String, trace: String)

    object ExtractionException {
      def apply(projectId: String, exception: Throwable): ExtractionException = {
        val cause = Option(exception.getCause).getOrElse(exception)
        val trace = cause.getStackTrace.head.toString

        ExtractionException(projectId, cause.getClass.getSimpleName, cause.getMessage, trace)
      }
    }

    private val messageOutput = messageFile.newOutputStream
    private val exceptionOutput =
      exceptionFile.newOutputStream
        .asCsvWriter[ExtractionException](rfc.withHeader("project_id", "cause", "message", "trace"))

    private val statusOutput =
      statusFile.newOutputStream
        .asCsvWriter[Status](
          rfc.withHeader("project_id", "failure", "declarations", "callsites", "failures"))

    def apply(projectId: String, result: Try[Result]): Stats = {
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
        .map(e => ExtractionException(projectId, e))
        .foreach(exceptionOutput.write)

      Stats(
        result.project.declarations.size,
        result.project.implicitCallSites.size,
        result.exceptions.size
      )
    }

    def close(): Unit = {
      Seq(messageOutput, exceptionOutput, statusOutput)
        .map(x => Try(x.close()))
        .foreach(_.get)
    }
  }

  class ImplicitExtractor extends (File => Result) {
    case class SubProjectResult(
        declarations: List[Declaration],
        implicitCallSites: List[CallSite],
        allCallSitesCount: Int,
        paths: List[PathEntry],
        exceptions: List[Throwable]
    )

    object SubProjectResult {
      implicit val monoid: Monoid[SubProjectResult] = {
        import derived.auto.monoid._
        derived.semi.monoid
      }
    }

    def apply(projectPath: File): Result = {

      val (metadata, warnings) = ProjectMetadata(projectPath)
      val subProjectResult =
        metadata.subProjects
          .foldLeft(Monoid[SubProjectResult].empty)((a, b) => a |+| extract(b))

      val project = Project(
        metadata.projectId,
        metadata.scalaVersion,
        metadata.sbtVersion,
        subProjectResult.paths.map(x => x.path -> x).toMap,
        subProjectResult.declarations,
        subProjectResult.implicitCallSites,
        subProjectResult.allCallSitesCount
      )

      Result(project, subProjectResult.exceptions ++ warnings)
    }

    def extract(metadata: SubProjectMetadata): SubProjectResult = {
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

      SubProjectResult(
        declarations = ctx.declarations,
        implicitCallSites = callSites,
        allCallSitesCount = csCount,
        paths = classpath ++ metadata.sourcepathEntries,
        declExceptions ++ csExceptions
      )
    }
  }

  def run(
      projectsPaths: Traversable[File],
      messageFile: File,
      exceptionFile: File,
      statusFile: File,
      threads: Int = Runtime.getRuntime.availableProcessors()): Unit = {

    val outputWriter = new OutputWriter(messageFile, exceptionFile, statusFile)
    val executor =
      new MultiProjectExecutor[Result, Stats](new ImplicitExtractor, outputWriter)
    try {
      // TODO: store the results into a CSV file
      executor.run(projectsPaths, threads)
    } finally {
      outputWriter.close()
    }
  }

  def run(source: String, outputPath: File): Unit = {
    val paths = File(source) match {
      case projectDir if projectDir.isDirectory => Seq(projectDir)
      case pathFile if pathFile.exists => pathFile.lines.map(x => Constants.ProjectsDirname / x)
      case projectName => Seq(Constants.ProjectsDirname / projectName.name)
    }

    val threads =
      Option(System.getenv("N_JOBS"))
        .map(_.toInt)
        .getOrElse(Runtime.getRuntime.availableProcessors())

    run(
      paths,
      outputPath / ExtractedImplicitsFilename,
      outputPath / ExtractionExceptionsFilename,
      outputPath / ExtractionStatusFilename,
      threads
    )
  }

  args.toList match {
    case source :: Nil => run(source, File("."))
    case source :: output :: Nil => run(source, File(output))
    case _ => sys.error("Usage: <source> <output>")
  }
}
