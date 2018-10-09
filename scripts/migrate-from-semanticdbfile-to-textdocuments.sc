import $ivy.`cz.cvut.fit.prl.scala.implicits::model:1.0-SNAPSHOT`
import $ivy.`cz.cvut.fit.prl.scala.implicits::transformation:1.0-SNAPSHOT`

import better.files._
import scala.meta.internal.semanticdb.TextDocuments
import cz.cvut.fit.prl.scala.implicits.merged.SemanticdbFile

import $file.commons
import commons._

def run(pathsFile: File): Unit = {
  val projects = pathsFile.lines.toList

  projects.zipWithIndex.foreach { case (name, idx) =>
    val projectPath = File(s"$RunDir/$name")
    val inputFile = File(s"$projectPath/$OutputDir/semanticdbs-4.0.0-M10.bin")

    print(s"Processing  $name [${idx + 1}/${projects.size}] ...")

    if (inputFile.exists) {
      val documents =
        inputFile
          .inputStream
          .apply(input => SemanticdbFile.streamFromDelimitedInput(input).map(_.sdb).toList)

      inputFile.outputStream.apply(output => documents.foreach(_.writeDelimitedTo(output)))
      println("done")
    } else {
      println("skipped")
    }
  }
}

@main
def main(pathsFilename: String) = {
  run(File(pathsFilename))
}
