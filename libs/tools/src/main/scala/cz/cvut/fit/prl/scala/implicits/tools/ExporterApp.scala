package cz.cvut.fit.prl.scala.implicits.tools

import better.files._

import cz.cvut.fit.prl.scala.implicits.model._
import cz.cvut.fit.prl.scala.implicits.model.Util._

trait ExporterApp[A, B] {
  protected def createExporter(): ProjectExporter[A, B]

  def run(inputFile: File): Unit = {
    val t = System.currentTimeMillis()
    for {
      input <- inputFile.inputStream
      exporter <- createExporter().autoClosed
      project <- Project.streamFrom(input)
    } exporter(project)
    println(System.currentTimeMillis()-t)
  }

  def main(args: Array[String]): Unit = {
    args.toList match {
      case input :: Nil => run(File(input))
      case _ => sys.error("Usage: <path/to/implicits.bin>")
    }
  }
}
