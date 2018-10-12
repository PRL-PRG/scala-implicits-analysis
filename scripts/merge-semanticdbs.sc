import $ivy.`cz.cvut.fit.prl.scala.implicits::model:1.0-SNAPSHOT`
import $ivy.`cz.cvut.fit.prl.scala.implicits::transformation:1.0-SNAPSHOT`
import $file.commons
import commons._

import java.nio.file.FileVisitOption
import java.util.concurrent.atomic.AtomicInteger

import better.files._
import cz.cvut.fit.prl.scala.implicits.utils.SdbLocator

import scala.collection.parallel._
import scala.concurrent.forkjoin.ForkJoinPool

val ExcludedDirs = Seq(".git", OutputDir)

def run(pathsFile: File): Unit = {
  val projects = pathsFile.lines.toList

  println(s"Processing ${projects.size} projects...")

  val parprojects = projects.par
  parprojects.tasksupport = new ForkJoinTaskSupport(new ForkJoinPool(5))
  val completed = new AtomicInteger(0)
  parprojects.foreach { name =>
    val projectPath = File(s"$RunDir/$name")
    val outputFile = File(s"$projectPath/$OutputDir/$MergedSemanticdbsFilename")

    val status = outputFile.outputStream.apply { output =>
      try {
        var n = 0
        new SdbLocator(projectPath.path)
          .exclude(ExcludedDirs)
          .options(FileVisitOption.FOLLOW_LINKS)
          .run { case (path, documents) =>
            documents.writeDelimitedTo(output)
            n = n + 1
          }
        s"($n)"
      } catch {
        case e: Throwable =>
          File(s"$outputFile.err").printWriter(true).apply(e.printStackTrace)
          s"(failed: ${e.getMessage})"
      }
    }

    println(s"Processed $name [${completed.incrementAndGet()}/${projects.size}] $status")
  }
}

@main
def main(pathsFile: String) = {
  run(File(pathsFile))
}
