package cz.cvut.fit.prl.scala.implicits.tools

import better.files._
import cats.Monoid
import cats.implicits._
import cats.derived
import cz.cvut.fit.prl.scala.implicits.Constants._
import cz.cvut.fit.prl.scala.implicits.{ProjectMetadata, ModuleMetadata}
import cz.cvut.fit.prl.scala.implicits.extractor.{
  CallSiteExtractor,
  DeclarationExtractor,
  ExtractionContext
}
import cz.cvut.fit.prl.scala.implicits.model._
import cz.cvut.fit.prl.scala.implicits.utils._
import kantan.csv._
import kantan.csv.ops._
import kantan.csv.generic._

import scala.util.{Failure, Success, Try}

object ExtractImplicits extends App {
  case class Stats(
      declarations: Int,
      callSites: Int,
      implicitDeclarations: Int,
      implicitCallSites: Int,
      failures: Int) {
    override def toString: String =
      s"$declarations, $callSites, $implicitDeclarations, $implicitCallSites, $failures"
  }

  object Stats {
    implicit val monoid: Monoid[Stats] = {
      import derived.auto.monoid._
      derived.semi.monoid
    }
  }

  case class Result(project: Project, exceptions: Seq[(String, Throwable)]) {
    def stats =
      Stats(
        project.modules.map(_.declarations.size).sum,
        project.modules.map(_.callSitesCount).sum,
        project.modules.map(_.declarations.count(_.isImplicit)).sum,
        project.modules.map(_.implicitCallSites.size).sum,
        exceptions.size)
  }

  class OutputWriter(messageFile: File, exceptionFile: File, statsFile: File) {

    sealed trait Status {
      def projectId: String
      def failure: String
      def stats: Stats
    }

    object Status {
      implicit val statusEncoder = RowEncoder.encoder(0, 1, 2, 3, 4, 5, 6)(
        (x: Status) =>
          (
            x.projectId,
            x.failure,
            x.stats.declarations,
            x.stats.callSites,
            x.stats.implicitDeclarations,
            x.stats.implicitCallSites,
            x.stats.failures))
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

    private val statsOutput =
      statsFile.newOutputStream
        .asCsvWriter[Status](
          rfc.withHeader(
            "project_id",
            "failure",
            "declarations",
            "callsites",
            "implicit_declarations",
            "implicit_callsites",
            "failures"))

    def write(projectId: String, result: Try[Result]): Stats = {
      val stats = result match {
        case Success(value) => SuccessfulExtraction(projectId, process(projectId, value))
        case Failure(exception) => FailedExtraction(projectId, exception)
      }

      statsOutput.write(stats)
      stats.stats
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
      Seq(messageOutput, exceptionOutput, statsOutput)
        .map(x => Try(x.close()))
        .foreach(_.get)
    }
  }

//  object Module {
//    implicit val monoid: Monoid[Module] = {
//      import derived.auto.monoid._
//      derived.semi.monoid
//    }
//  }
//
  def extractProject(projectPath: File): Result = {
    val (metadata, warnings) = ProjectMetadata(projectPath)
    val modulesResult = metadata.modules.map(extractModule)

    val project = Project(
      metadata.projectId,
      metadata.sbtVersion,
      modulesResult.map(_._1)
    )

    val modulesExceptions = modulesResult
      .flatMap {
        case (module, exceptions) => exceptions.map(e => module.moduleId -> e)
      }

    Result(
      project,
      modulesExceptions ++ warnings.map(x => "ROOT" -> x)
    )
  }

  def extractModule(metadata: ModuleMetadata): (Module, List[Throwable]) = {
    val ctx = new ExtractionContext(metadata.resolver, metadata.sourcepathEntries.map(_.path))
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

    val classpath =
        metadata.classpathEntries ++ Libraries.JvmBootModelClasspath

    val exceptions = declExceptions ++ csExceptions

    val paths =
      (classpath.map(x => x.path -> x) ++
        metadata.sourcepathEntries.map(x => x.path -> x)).toMap

    val module = Module(
      moduleId = metadata.moduleId,
      groupId = metadata.groupId,
      artifactId = metadata.artifactId,
      version = metadata.version,
      scalaVersion = metadata.scalaVersion,
      paths = paths,
      declarations = ctx.declarations,
      implicitCallSites = callSites,
      callSitesCount = csCount,
      outputPath = metadata.outputPath,
      testOutputPath = metadata.testOutputPath
    )

    (module, exceptions)
  }

  def run(projectPath: File, outputPath: File): Unit = {
    val messageFile = outputPath / ExtractedImplicitsFilename
    val exceptionFile = outputPath / ExtractionExceptionsFilename
    val statsFile = outputPath / ExtractionStatsFilename

    val outputWriter = new OutputWriter(messageFile, exceptionFile, statsFile)

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
