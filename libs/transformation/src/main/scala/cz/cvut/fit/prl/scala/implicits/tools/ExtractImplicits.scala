package cz.cvut.fit.prl.scala.implicits.tools
import java.io.{OutputStream, PrintStream, PrintWriter}

import better.files._
import cats.Monoid
import cz.cvut.fit.prl.scala.implicits.Constants._
import cz.cvut.fit.prl.scala.implicits.ProjectMetadata
import cz.cvut.fit.prl.scala.implicits.extractor.{CallSiteExtractor, ConversionException, DeclarationExtractor, ExtractionContext}
import cz.cvut.fit.prl.scala.implicits.model.{Path, Project}
import cz.cvut.fit.prl.scala.implicits.utils.{MultiProjectExecutor, _}

object ExtractImplicits extends App {

  case class Result(declarations: Int, callSites: Int, failures: List[String])
      extends Reporting {

    override def writeReport(writer: PrintWriter): Unit = {
      if (failures.nonEmpty) {
        writer.println("Failure summary:")
        failures.printGroups(x => x, writer)
      }

      println()

      println(s"Declarations: $declarations")
      println(s"Call sites: $callSites")
      println(s"Failures: ${failures.size}")
    }

    override def status: String = s"$declarations, $callSites, ${failures.size}"
  }

  object Result {
    implicit val resultMonoid: Monoid[Result] = new Monoid[Result] {
      def empty: Result = Result(0, 0, Nil)

      def combine(x: Result, y: Result): Result =
        Result(
          x.declarations + y.declarations,
          x.callSites + y.callSites,
          x.failures ++ y.failures)
    }
  }

  def run(
      projectsFile: File,
      outputFile: File,
      exceptionFile: File,
      threads: Int = Runtime.getRuntime.availableProcessors()): Unit = {
    val projects = projectsFile.lines.map(x => ProjectsDirname / x).toList
    val result = for {
      messageOutput <- outputFile.newOutputStream.autoClosed
      exceptionOutput <- exceptionFile.newOutputStream.autoClosed
    } yield
      new MultiProjectExecutor(new Task(messageOutput, exceptionOutput), threads)(
        Result.resultMonoid).run(projects)

    result.get().printSummary()
  }

  class Task(messageOutput: OutputStream, exceptionWriter: OutputStream)
      extends (File => Result) {

    override def apply(projectPath: File): Result = {
      val metadata = new ProjectMetadata(projectPath)
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

      val classpath =
        metadata.classpathEntries.map(x =>
          Path(x.path, Path.Kind.CLASSPATH, x.scope, managed = false))

      val sourcepath =
        metadata.sourcepathEntries.map(x =>
          Path(x.path, Path.Kind.SOURCEPATH, x.scope, x.managed))

      val project = Project(
        projectId = metadata.projectId,
        declarations = ctx.declarations,
        implicitCallSites = callSites,
        allCallSitesCount = csCount,
        scalaVersion = metadata.scalaVersion,
        sbtVersion = metadata.sbtVersion,
        paths = classpath ++ sourcepath
      )

      val allExceptions = (declExceptions ++ csExceptions) collect {
        case x: ConversionException => x
      }

      synchronized {
        val pstream = new PrintStream(exceptionWriter)
        pstream.println(s"** ${project.projectId}\n")
        project.writeDelimitedTo(messageOutput)
        allExceptions.foreach(_.printStackTrace(pstream))
      }

      Result(project.declarations.size, callSites.size, allExceptions.map(_.summary))
    }
  }

  run(File(args(0)), File(ExtractedImplicitsFilename), File(ExtractionExceptionsFilename))
}
