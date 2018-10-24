package cz.cvut.fit.prl.scala.implicits.tools
import java.nio.file.FileVisitOption

import better.files._
import cats.instances.int._
import cz.cvut.fit.prl.scala.implicits.Constants
import cz.cvut.fit.prl.scala.implicits.utils.{MultiProjectExecutor, SdbLocator}

object MergeSemanticdbs extends App {

  def run(projectsFile: File): Unit = {
    val projects = projectsFile.lines.map(x => Constants.ProjectsDirname / x).toList
    val result = new MultiProjectExecutor(Task).run(projects)
    result.printSummary()
  }

  object Task extends (File => Int) {
    override def apply(projectPath: File): Int = {
      val outputFile = projectPath / Constants.AnalysisDirname / Constants.MergedSemanticdbFilename

      outputFile.outputStream.apply { output =>
        var n = 0
        new SdbLocator(projectPath.path)
          .exclude(Constants.ExcludedDirs)
          .options(FileVisitOption.FOLLOW_LINKS)
          .run {
            case (_, db) =>
              db.documents.foreach(_.writeDelimitedTo(output))
              n = n + 1
          }
        n
      }
    }
  }

  run(File(args(0)))
}
