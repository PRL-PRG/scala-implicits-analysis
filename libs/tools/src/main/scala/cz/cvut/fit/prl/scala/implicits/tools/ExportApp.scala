package cz.cvut.fit.prl.scala.implicits.tools

import better.files._

import cz.cvut.fit.prl.scala.implicits.model.Index

import kantan.csv._
import kantan.csv.ops._
import kantan.csv.generic._

import scala.collection.mutable

trait ExportApp {

  case class Problem(
      projectId: String,
      moduleId: String,
      message: String,
      exception: String,
      exceptionMessage: String,
      exceptionTrace: String
  )

  object Problem {
    val Header = Seq(
      "project_id",
      "module_id",
      "message",
      "exception",
      "exception_message",
      "exception_trace"
    )
  }

  private val problems = mutable.Buffer[Problem]()

  protected def defaultOutputFilename: String
  protected def run(idx: Index, outputFile: File): Unit

  protected def reportException(
      projectId: String,
      moduleId: String,
      message: String,
      e: Throwable): Unit = {

    val problem = Problem(
      projectId,
      moduleId,
      message,
      e.getClass.getSimpleName,
      e.getMessage,
      e.getStackTrace.head.toString)

    problems += problem
  }

  private def writeProblems(): Unit =
    for {
      out <- File(File(defaultOutputFilename).nameWithoutExtension + "-problems.csv").newOutputStream.autoClosed
      writer <- out.asCsvWriter[Problem](rfc.withHeader(Problem.Header: _*)).autoClosed
      problem <- problems
    } writer.write(problem)

  protected def outputFile: File = File.currentWorkingDirectory / defaultOutputFilename

  def run(index: Index): Unit = {
    run(index, outputFile)
  }

  def run(inputFile: File): Unit = {
    val index = Index.fromFile(inputFile)

    try {
      run(index, outputFile)
    } finally {
      writeProblems()
    }
  }

  def main(args: Array[String]): Unit = {
    args.toList match {
      case input :: Nil => run(File(input))
      case _ => sys.error("Usage: <path/to/implicits.bin>")
    }
  }
}
