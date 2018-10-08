import $ivy.`cz.cvut.fit.prl.scala.implicits::model:1.0-SNAPSHOT`
import $ivy.`cz.cvut.fit.prl.scala.implicits::transformation:1.0-SNAPSHOT`

import java.nio.file.FileVisitOption

import better.files._
import cz.cvut.fit.prl.scala.implicits.merged.SemanticdbFile
import cz.cvut.fit.prl.scala.implicits.utils.SdbLocator

import scala.meta.internal.semanticdb.TextDocuments

def run(startPath: File, outputFile: File): Unit = {
  outputFile.outputStream.apply { output =>
    SdbLocator(startPath.path, FileVisitOption.FOLLOW_LINKS) { case (path, documents) =>
      val msg = SemanticdbFile(path.toAbsolutePath.toString, documents)
      msg.writeDelimitedTo(output)
      println(s"Processed $path")
    }
  }
}

@main
def main(startPath: String, outputFile: String) = {
  run(File(startPath), File(outputFile))
}
