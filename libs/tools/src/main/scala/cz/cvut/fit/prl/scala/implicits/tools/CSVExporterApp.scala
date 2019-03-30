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

abstract class CSVExporterApp[T: HeaderEncoder](exporter: Exporter[T], defaultOutputFile: File) {
  implicit val logger: Logger = Logger(LoggerFactory.getLogger(getClass.getName))

  def csvExporter: CSVExporter[T] = new CSVExporter[T](exporter, defaultOutputFile)

  protected def run(inputFile: File): Unit = {
    timedTask(s"Exporting projects from $inputFile") {
      for {
        input <- inputFile.inputStream
        csv <- csvExporter.autoClosed
        project <- Project.streamFrom(input)
      } {
        timedTask(s"Exporting project ${project.projectId}") {
          csv.export(project)
        }
      }
    }
  }

  def main(args: Array[String]): Unit = {
    args.toList match {
      case input :: Nil => run(File(input))
      case _ => sys.error("Usage: <path/to/implicits.bin>")
    }
  }
}