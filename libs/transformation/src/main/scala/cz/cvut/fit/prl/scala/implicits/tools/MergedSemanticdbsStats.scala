package cz.cvut.fit.prl.scala.implicits.tools

import better.files._
import cats.instances.all._
import cz.cvut.fit.prl.scala.implicits.Constants._
import cz.cvut.fit.prl.scala.implicits.ProjectMetadata
import cz.cvut.fit.prl.scala.implicits.utils._

import scala.meta.internal.semanticdb.TextDocument

object MergedSemanticdbsStats extends App {

  def run(projectsFile: File): Unit = {
    val projects = projectsFile.lines.map(x => ProjectsDirname / x).toList
    val result = new MultiProjectExecutor(Task).run(projects)
    result.printSummary()
  }

  object Task extends (File => (Int, Int, Int)) {
    override def apply(projectPath: File): (Int, Int, Int) = {
      val projectMetadata = new ProjectMetadata(projectPath)
      val inputFile = projectMetadata.metadataFile(MergedSemanticdbFilename)
      val outputFile = projectMetadata.metadataFile(MergedSemanticdbStatsFilename)

      if (inputFile.exists) {
        val semanticdbs =
          inputFile.inputStream
            .apply(input => TextDocument.streamFromDelimitedInput(input).toList)

        val stats = (
          semanticdbs.map(x => x.occurrences.size).sum,
          semanticdbs.map(x => x.synthetics.size).sum,
          semanticdbs.map(x => x.symbols.size).sum
        )

        val csv =
          s"""
            |project,occurrencies,synthtics,symbols
            |${projectPath.name},${stats._1},${stats._2},${stats._3}
            |""".stripMargin

        outputFile.writeText(csv)

        stats
      } else {
        (0, 0, 0)
      }
    }
  }

  run(File(args(0)))
}
