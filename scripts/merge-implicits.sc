import $ivy.`cz.cvut.fit.prl.scala.implicits::tools:1.0-SNAPSHOT`
import $ivy.`cz.cvut.fit.prl.scala.implicits::metadata:1.0-SNAPSHOT`
import $ivy.`org.typelevel::kittens:1.2.0`

import better.files._

import cats.Monoid
import cats.syntax.monoid._

import scala.util.Try
import scala.util.Success
import scala.util.Failure

import kantan.csv._
import kantan.csv.ops._
import kantan.csv.generic._

import cz.cvut.fit.prl.scala.implicits.metadata.MetadataFilenames._
import cz.cvut.fit.prl.scala.implicits.model.Index

case class FailedProject(
                          projectId: String,
                          exception: String,
                          message: String,
                          trace: String,
                          cause: String,
                          causeMessage: String,
                          causeTrace: String
                        )

object FailedProject {
  val Header = Seq(
    "project_id",
    "exception",
    "message",
    "trace",
    "cause",
    "cause_message",
    "cause_trace"
  )

  def apply(projectId: String, e: Throwable): FailedProject = new FailedProject(
    projectId,
    e.getClass.getSimpleName,
    Option(e.getMessage).getOrElse("NA"),
    e.getStackTrace.head.toString,
    Option(e.getCause).map(_.getClass.getSimpleName).getOrElse("NA"),
    Option(e.getCause).flatMap(x => Option(x.getMessage)).getOrElse("NA"),
    Option(e.getCause).map(_.getStackTrace.head.toString).getOrElse("NA"),
  )
}

def build(path: File): (List[FailedProject], Index) = {
  if (!path.isDirectory) {
    throw new IllegalArgumentException(s"$path must be a directory")
  }

  val (failures, index) = {
    val indexes =
      for {
        projectName <- (File.currentWorkingDirectory / ProjectsFilename).lineIterator
        projectPath = path / ProjectsDirname / projectName
        dataFile = projectPath / AnalysisDirname / ExtractedImplicitsFilename if dataFile.exists
      } yield
        Try(Index.fromProjectFile(dataFile)) match {
          case Success(v) => Right(v)
          case Failure(e) => Left(projectName -> e)
        }

    val empty = (List[FailedProject](), Monoid[Index].empty)

    indexes.foldLeft(empty) {
      case ((fs, idx), b) =>
        b match {
          case Right(v) => (fs, idx |+| v)
          case Left((projectId, e)) => (FailedProject(projectId, e) :: fs, idx)
        }
    }
  }

  println(
    s"Loaded index from: $path (" +
      s"failures: ${failures.size}, " +
      s"projects: ${index.projects.size}, " +
      s"modules: ${index.modules.size}, " +
      s"call sites: ${index.implicitCallSites.size}, " +
      s"declarations: ${index.implicitDeclarations.size}" +
      s")"
  )

  (failures, index)
}

@main
def main() = {
  val baseDir = File.currentWorkingDirectory

  val (failures, index) = build(baseDir)

  Index.saveToProjectsFile(index, baseDir / ExtractedImplicitsFilename)

  val indexExceptionFile = baseDir / ExtractionIndexExceptionsFilename

  for {
    out <- indexExceptionFile.newOutputStream.autoClosed
    writer <- out.asCsvWriter[FailedProject](rfc.withHeader(FailedProject.Header: _*)).autoClosed
    failure <- failures
  } writer.write(failure)
}
