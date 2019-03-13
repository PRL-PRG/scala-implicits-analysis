package cz.cvut.fit.prl.scala.implicits.tools

import better.files.File

import cz.cvut.fit.prl.scala.implicits.model.Index

trait ExportApp {

  protected def defaultOutputFilename: String
  protected def run(idx: Index, outputFile: File): Unit

  def run(index: Index): Unit = {
    val outputFile = File.currentWorkingDirectory / defaultOutputFilename

    run(index, outputFile)
  }

  def run(corpusPath: File): Unit = {
    val index: Index = Index.fromProjectsFile(corpusPath)
    val outputFile = corpusPath / defaultOutputFilename

    run(index, outputFile)
  }

  def main(args: Array[String]): Unit = {
    args.toList match {
      case output :: Nil => run(File(output))
      case _ => sys.error("Usage: <corpus path>")
    }
  }
}
