package cz.cvut.fit.prl.scala.implicits.tools

import java.io.OutputStream

import better.files._

import cz.cvut.fit.prl.scala.implicits.model._

import cats.Monoid
import cats.implicits._

import kantan.csv._
import kantan.csv.ops._
import kantan.csv.generic._

import scala.util.Try

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

trait Writer[A] extends AutoCloseable {
  def write(x: A): Unit
}

class CSVWriter[A: HeaderEncoder](outputFile: File, csvHeader: Iterable[String]) extends Writer[A] {
  private var output: OutputStream = _
  private var writer: CsvWriter[A] = _

  private def open(): Unit = {
    output = outputFile.newOutputStream
    writer = output.asCsvWriter[A](rfc.withHeader(csvHeader.toSeq: _*))
  }

  override def write(x: A): Unit = {
    if (output == null) open()
    writer.write(x)
  }

  override def close(): Unit = {
    val wc = Try(if (writer != null) writer.close())
    val oc = Try(if (output != null) output.close())

    if (wc.isFailure) wc.get
    if (oc.isFailure) oc.get
  }
  override def toString: String = outputFile.pathAsString
}

class ProjectExporter[A, B](
    writer: Writer[B],
    problemWriter: Writer[Problem],
    indexer: Index => Iterator[A],
    mapper: (A, Index) => Either[Problem, B]
) extends (Project => Unit)
    with AutoCloseable {
  type Stat = (Int, Int)

  var stat: Stat = Monoid[Stat].empty

  override def apply(project: Project): Unit = {
    val index = FullIndex(project)

    val pipe =
      indexer(index).map { item =>
        mapper(item, index) match {
          case Right(row) =>
            writer.write(row)
            (1, 0)
          case Left(problem) =>
            problemWriter.write(problem)
            (0, 1)
        }
      }

    stat = pipe.foldLeft(stat)(_ |+| _)
  }

  override def close(): Unit = {
    println(s"Exported ${stat._1} items (failed: ${stat._2}) into $writer")
    val es = Seq(Try(writer.close()), Try(problemWriter.close()))
    es.foreach(_.get)
  }
}
