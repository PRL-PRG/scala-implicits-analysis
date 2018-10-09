import $ivy.`cz.cvut.fit.prl.scala.implicits::model:1.0-SNAPSHOT`
import $ivy.`cz.cvut.fit.prl.scala.implicits::transformation:1.0-SNAPSHOT`

import java.nio.file.FileVisitOption

import better.files._
import cz.cvut.fit.prl.scala.implicits.utils.SdbLocator

import $file.commons
import commons._

val ExcludedDirs = Seq(".git", OutputDir)

def run(pathsFile: File): Unit = {
  val projects = pathsFile.lines.toList

  projects.zipWithIndex.foreach { case (name, idx) =>
    val projectPath = File(s"$RunDir/$name")
    val outputFile = File(s"$projectPath/$OutputDir/$MergedSemanticdbsFilename")

    print(s"Processing $name [${idx + 1}/${projects.size}] ")

    var n = 0
    outputFile.outputStream.apply { output =>
      new SdbLocator(projectPath.path)
        .exclude(ExcludedDirs)
        .options(FileVisitOption.FOLLOW_LINKS)
        .run { case (path, documents) =>
          documents.writeDelimitedTo(output)
          n = n + 1

          print(".")
        }
    }

    println(s" ($n)")
  }
}

@main
def main(pathsFile: String) = {
  run(File(pathsFile))
}
