package cz.cvut.fit.prl.scala.implicits.tools
import java.io.{OutputStream, PrintWriter}

import better.files._
import cats.Monoid
import cz.cvut.fit.prl.scala.implicits.Constants._
import cz.cvut.fit.prl.scala.implicits.ProjectMetadata
import cz.cvut.fit.prl.scala.implicits.extractor.{
  CallSiteExtractor,
  ConversionException,
  DeclarationExtractor,
  ExtractionContext
}
import cz.cvut.fit.prl.scala.implicits.model.{Project, SourcePath}
import cz.cvut.fit.prl.scala.implicits.utils.{MultiProjectExecutor, _}

object ExtractImplicits extends App {

  case class Result(
      declarations: Int,
      callSites: Int,
      exceptions: List[ConversionException])
      extends Reporting {
    override def writeReport(writer: PrintWriter): Unit = {
      if (exceptions.nonEmpty) {
        exceptions.foreach { x =>
          writer.println(x.longSummary)
          x.getCause.printStackTrace(writer)
          writer.println()
        }

        writer.println("Failure summary:")
        exceptions.printGroups(_.summary, writer)
      }

      println()

      println(s"Declarations: $declarations")
      println(s"Call sites: $callSites")
      println(s"Exception: ${exceptions.size}")
    }

    override def status: String = s"$declarations, $callSites, ${exceptions.size}"
  }

  object Result {
    implicit val resultMonoid: Monoid[Result] = new Monoid[Result] {
      def empty: Result = Result(0, 0, Nil)

      def combine(x: Result, y: Result): Result =
        Result(
          x.declarations + y.declarations,
          x.callSites + y.callSites,
          x.exceptions ++ y.exceptions)
    }
  }

  def run(projectsFile: File, outputFile: File, threads: Int = 1): Unit = {
    val projects = projectsFile.lines.map(x => ProjectsDirname / x).toList
    val result = outputFile.outputStream.apply { output =>
      new MultiProjectExecutor(new Task(output), threads)(Result.resultMonoid)
        .run(projects)
    }

    result.printSummary()
  }

  class Task(output: OutputStream) extends (File => Result) {
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
          .flatMap(csExtractor.extractImplicitCallSites)
          .split()

      val csCount = metadata.semanticdbs.map(csExtractor.callSiteCount).sum

      val project = Project(
        projectId = metadata.projectId,
        declarations = ctx.declarations,
        implicitCallSites = callSites,
        allCallSitesCount = csCount,
        scalaVersion = metadata.scalaVersion,
        sbtVersion = metadata.sbtVersion,
        classpaths = metadata.classpathEntries.map(_.path),
        sourcepaths = metadata.sourcepathEntries.map(x => SourcePath(x.path, x.kind, x.scope))
      )

      project.writeDelimitedTo(output)

      val allExceptions = (declExceptions ++ csExceptions) collect {
        case x: ConversionException => x
      }

      Result(project.declarations.size, callSites.size, allExceptions)
    }
  }

  run(File(args(0)), File(ExtractedImplicitsFilename))
}
