package cz.cvut.fit.prl.scala.implicits.tools
import java.io.OutputStream
import java.nio.file.FileVisitOption

import better.files._
import cats.instances.all._
import cz.cvut.fit.prl.scala.implicits.Constants._
import cz.cvut.fit.prl.scala.implicits.mergedsdb._
import cz.cvut.fit.prl.scala.implicits.ProjectMetadata
import cz.cvut.fit.prl.scala.implicits.utils.{MultiProjectExecutor, SdbLocator}

object MergeSemanticdbs extends App {

  def run(projectsFile: File, outputFile: File, threads: Int = 10): Unit = {
    val projects = projectsFile.lines.map(x => ProjectsDirname / x).toList
    val result = outputFile.outputStream.apply { output =>
      new MultiProjectExecutor(new Task(output), threads).run(projects)
    }
    result.printSummary()
  }

  class Task(output: OutputStream) extends (File => (Int, Int)) {
    override def apply(projectPath: File): (Int, Int) = {
      val project = new ProjectMetadata(projectPath)

      if (!project.mergedSemanticdbFile.exists) {
        project.mergeSemanticdbs()
      }

      val semanticdbs = project.semanticdbs
      val classpaths = project.classpathEntries.map(_.path)
      val sourcepaths = project.sourcepathEntries.map(x => SourcePath(x.path, x.kind, x.scope))

      val merged = MergedSemanticdbs(
        projectId = project.projectId,
        classpaths = classpaths,
        sourcepaths = sourcepaths,
        semanticdbs = semanticdbs
      )

      merged.writeDelimitedTo(output)

      (1, semanticdbs.size)
    }
  }

  run(File(args(0)), File(MergedSemanticdbFilename))
}
