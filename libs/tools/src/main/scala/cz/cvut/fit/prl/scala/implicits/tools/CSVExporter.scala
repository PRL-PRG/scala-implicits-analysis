package cz.cvut.fit.prl.scala.implicits.tools

import java.io.OutputStream

import better.files._
import cz.cvut.fit.prl.scala.implicits.model._
import kantan.csv._
import kantan.csv.ops._
import scalapb.GeneratedEnum

import scala.util.{Failure, Success, Try}

object CSVExporter {

  object encoders {
    import scala.language.implicitConversions

    case class Value(str: String)
    val NA = Value("NA")

    trait ValueEncoder[T] {
      def value(x: T): Value
    }

    implicit def toValue[T: ValueEncoder](x: T) = implicitly[ValueEncoder[T]].value(x)

    implicit val BooleanValueEncoder: ValueEncoder[Boolean] =
      (x: Boolean) => if (x) Value("TRUE") else Value("FALSE")

    implicit val StringEncoder: ValueEncoder[String] =
      (x: String) => Value(x)

    implicit val IntEncoder: ValueEncoder[Int] =
      (x: Int) => x.toString

    implicit val PositionEncoder: ValueEncoder[Position] =
      (x: Position) => x.startLine + ":" + x.startCol

    implicit def GeneratedEnumEncoder[T <: GeneratedEnum]: ValueEncoder[T] =
      (x: T) => x.name

    implicit def seqEncoder[T: ValueEncoder]: ValueEncoder[Seq[T]] =
      (x: Seq[T]) =>
        if (x.isEmpty) NA
        else {
          val enc = implicitly[ValueEncoder[T]]
          x.map(v => enc.value(v).str).mkString(";")
      }

    implicit def OptionEncoder[T: ValueEncoder]: ValueEncoder[Option[T]] =
      (x: Option[T]) => {
        val enc = implicitly[ValueEncoder[T]]
        x.map(v => enc.value(v)).getOrElse(NA)
      }

    implicit class ValueToAny[T: ValueEncoder](x: T) {
      def toValue: Value = implicitly[ValueEncoder[T]].value(x)
    }
  }
}

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

    import ExportError.implicits.encoder
    errorWriter = errorOutput.asCsvWriter[ExportError](CsvConfiguration.rfc.withHeader)
  }

  override def export(project: Project): Unit = {
    if (writer == null) open()

    exporter.export(project).foreach {
      case Success(value) =>
        write(project.projectId, value)
      case Failure(e) =>
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
