package cz.cvut.fit.prl.scala.implicits.tools

import better.files._
import cats.Monoid
import cats.implicits._
import cats.derived
import cz.cvut.fit.prl.scala.implicits.extractor.GlobalSymbolTable.ScalaSyntheticsLocation
import cz.cvut.fit.prl.scala.implicits.metadata.MetadataFilenames._
import cz.cvut.fit.prl.scala.implicits.{ModuleMetadata, ProjectMetadata}
import cz.cvut.fit.prl.scala.implicits.extractor.{CallSiteExtractor, DeclarationExtractor, ExtractionContext, GlobalSymbolTable}
import cz.cvut.fit.prl.scala.implicits.model._
import cz.cvut.fit.prl.scala.implicits.model.Util._
import cz.cvut.fit.prl.scala.implicits.utils._
import kantan.csv._
import kantan.csv.ops._
import kantan.csv.generic._

import scala.util.{Failure, Success, Try}

object ExtractImplicits extends App {
  case class Stats(
      declarations: Int,
      implicitDeclarations: Int,
      localImplicitDeclarations: Int,
      callSites: Int,
      testCallSites: Int,
      implicitCallSites: Int,
      failures: Int) {
    override def toString: String =
      s"$declarations, $implicitDeclarations, $localImplicitDeclarations, $callSites, $testCallSites, $implicitCallSites, $failures"
  }

  object Stats {
    implicit val monoid: Monoid[Stats] = {
      import derived.auto.monoid._
      derived.semi.monoid
    }
  }

  // TODO: use types for ModuleId
  case class Result(project: Project, exceptions: Seq[(String, Throwable)]) {
    def stats = {
      val modules = project.modules.values
      val implicitDeclarations = modules.flatMap(_.implicitDeclarations)

      Stats(
        modules.map(_.declarations.size).sum,
        implicitDeclarations.size,
        implicitDeclarations.count(_.isProjectLocal),
        modules.map(_.callSitesCount).sum,
        modules.map(_.testCallSitesCount).sum,
        modules.map(_.implicitCallSites.size).sum,
        exceptions.size
      )
    }
  }

  class OutputWriter(messageFile: File, exceptionFile: File, statsFile: File) {

    sealed trait Status {
      def projectId: String
      def failure: String
      def stats: Stats
    }

    object Status {
      // TODO: provide a custom one, this is ugly
      implicit val statusEncoder = RowEncoder.encoder(0, 1, 2, 3, 4, 5, 6, 7, 8)(
        (x: Status) =>
          (
            x.projectId,
            x.failure,
            x.stats.declarations,
            x.stats.implicitDeclarations,
            x.stats.localImplicitDeclarations,
            x.stats.callSites,
            x.stats.testCallSites,
            x.stats.implicitCallSites,
            x.stats.failures
          )
      )
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
        exception: String,
        message: String,
        trace: String,
        cause: String,
        causeMessage: String,
        causeTrace: String
    )

    object ExtractionException {
      def apply(projectId: String, moduleId: String, exception: Throwable): ExtractionException = {
        val cause = Option(exception.getCause).getOrElse(exception)
        val trace = cause.getStackTrace.head.toString

        val nestedCause = Option(cause.getCause)
        val nestedCauseMessage = nestedCause.map(_.getMessage)
        val nestedTrace = nestedCause.map(_.getStackTrace.head.toString)

        ExtractionException(
          projectId,
          moduleId,
          cause.getClass.getSimpleName,
          cause.getMessage,
          trace,
          nestedCause.map(_.getClass.getSimpleName).getOrElse("NA"),
          nestedCauseMessage.getOrElse("NA"),
          nestedTrace.getOrElse("NA")
        )
      }
    }

    private val messageOutput = messageFile.newOutputStream
    private val exceptionOutput =
      exceptionFile.newOutputStream
        .asCsvWriter[ExtractionException](
          rfc.withHeader(
            "project_id",
            "module_id",
            "exception",
            "message",
            "trace",
            "cause",
            "cause_message",
            "cause_trace"))

    private val statsOutput =
      statsFile.newOutputStream
        .asCsvWriter[Status](
          rfc.withHeader(
            "project_id",
            "failure",
            "declarations",
            "implicit_declarations",
            "implicit_local_declarations",
            "callsites",
            "test_callsites",
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
      Project.write(result.project, messageOutput)
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

  def extractProject(projectPath: File): Result = {
    val (metadata, warnings) = ProjectMetadata(projectPath)
    val modulesResult = metadata.modules.map(extractModule(metadata.projectId))

    val project = Project(
      metadata.projectId,
      metadata.sbtVersion,
      modulesResult.map {
        case (module, _) => module.moduleId -> module
      }.toMap
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

  def extractModule(projectId: String)(metadata: ModuleMetadata): (Module, List[Throwable]) = {
    val ctx = new ExtractionContext(
      metadata.moduleId,
      metadata.resolver,
      metadata.sourcepathEntries.map(_.path))
    val declExtractor = new DeclarationExtractor(ctx)
    val csExtractor = new CallSiteExtractor(ctx)

    val (_, declExceptions) =
      metadata.semanticdbs
        .flatMap(declExtractor.extractImplicitDeclarations)
        .split()

    val (callSites, csExceptions) =
      metadata.semanticdbs
        .map(x => x -> metadata.ast(x.uri))
        .flatMap {
          case (db, ast) => csExtractor.extractImplicitCallSites(metadata.moduleId, db, ast)
        }
        .split()

    val classpath =
      metadata.classpathEntries ++ Libraries.JvmBootModelClasspath

    val exceptions = declExceptions ++ csExceptions

    def classPathEntry(path: String, scope: String): ClasspathEntry =
      ClasspathEntry(
        path,
        metadata.groupId,
        metadata.artifactId,
        metadata.version,
        scope,
        internal = true,
        managed = false,
        transitive = false
      )

    val allPaths =
      classpath.map(x => x.path -> x) ++
        metadata.sourcepathEntries.map(x => x.path -> x) ++
        metadata.outputPath.map(x => x -> classPathEntry(x, "compile")) ++
        metadata.testOutputPath.map(x => x -> classPathEntry(x, "test")) ++
        List(ScalaSyntheticsLocation.path -> Libraries.ScalaModelClaspathEntry.withPath(ScalaSyntheticsLocation.path))

    val paths =
      allPaths
        .foldLeft(Map[String, PathEntry]()) {
          case (res, entry @ (path, _)) =>
            res.get(path) match {
              case Some(x) if x.scope == "test" =>
                res + entry
              case Some(_) => res
              case None =>
                res + entry
            }
        }

    val (csCount, testCsCount) =
      metadata.semanticdbs.foldLeft((0,0)) {
        case ((c, t), db) =>
          val ast = metadata.ast(db.uri)
          val count = csExtractor.callSiteCount(ast)(db)

          paths.find(x => db.uri.startsWith(x._1)) match {
            case Some((_, SourcepathEntry(_, "test", _))) => (c, t + count)
            case _ => (c + count, t)
          }
      }

    val module = Module(
      projectId = projectId,
      moduleId = metadata.moduleId,
      groupId = metadata.groupId,
      artifactId = metadata.artifactId,
      version = metadata.version,
      commit = metadata.commit,
      scalaVersion = metadata.scalaVersion,
      paths = paths,
      declarations = ctx.declarations.map(x => x.declarationId -> x).toMap,
      implicitCallSites = callSites,
      callSitesCount = csCount + testCsCount, // callSitesCount contains all call sites
      testCallSitesCount = testCsCount // out of which testCallSitesCount is found in test scope
    )

    module.declarations.foreach {
      case (_, d) => if (d.signature.isEmpty) {
        throw new Exception(s"Declaration $d has an empty signature")
      }
    }

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
