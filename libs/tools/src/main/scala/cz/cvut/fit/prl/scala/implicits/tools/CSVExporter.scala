package cz.cvut.fit.prl.scala.implicits.tools

import java.io.OutputStream

import better.files._
import cats.Monoid
import cats.implicits._
import com.typesafe.scalalogging.Logger
import cz.cvut.fit.prl.scala.implicits.model._
import cz.cvut.fit.prl.scala.implicits.model.Util._
import kantan.csv._
import kantan.csv.ops._
import kantan.csv.generic._
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

class CSVExporter[T: HeaderEncoder](
    exporter: Exporter[T],
    outputFile: File,
    errorOutputFile: File
) extends MultiProjectExporter {

  private var output: OutputStream = _
  private var errorOutput: OutputStream = _
  private var writer: CsvWriter[T] = _
  private var errorWriter: CsvWriter[ExportError] = _

  def this(exporter: Exporter[T], outputFile: File) =
    this(exporter, outputFile, File(outputFile.nameWithoutExtension + "-problems.csv"))

  private def open(): Unit = {
    output = outputFile.newOutputStream
    writer = output.asCsvWriter[T](CsvConfiguration.rfc.withHeader)
    errorOutput = errorOutputFile.newOutputStream
    errorWriter = errorOutput.asCsvWriter[ExportError](CsvConfiguration.rfc.withHeader)
  }

  override def export(project: Project): Unit = {
    if (writer == null) open()

    exporter.export(project).foreach {
      case Success(value) =>
        write(project.projectId, value)
      case Failure(e) =>
        e.printStackTrace()
        errorWriter.write(
          ExportError(
            project.projectId,
            "",
            "",
            e.getClass.getSimpleName,
            e.getMessage,
            e.getStackTrace.head.toString
          )
        )
    }
  }

  private def write(projectId: String, value: T): Unit = Try(writer.write(value)) match {
    case Success(_) =>
    case Failure(e) =>
      errorWriter.write(
        ExportError(
          projectId,
          "",
          "Unable to write record",
          e.getClass.getSimpleName,
          e.getMessage,
          e.getStackTrace.head.toString
        )
      )
  }

  override def close(): Unit = {
    val closable = Seq(writer, errorWriter, output, errorOutput)
    closable.map(x => Try { if (x != null) x.close() }).foreach(_.get)
  }
}
