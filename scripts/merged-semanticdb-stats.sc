import $ivy.`cz.cvut.fit.prl.scala.implicits::model:1.0-SNAPSHOT`
import $ivy.`cz.cvut.fit.prl.scala.implicits::transformation:1.0-SNAPSHOT`

import better.files._
import cz.cvut.fit.prl.scala.implicits.merged.SemanticdbFile

import scala.meta.internal.semanticdb.TextDocuments

val LeftPathMatcher = "scala-corpus/projects/"
val RightPathMatcher = "/target/scala-"

def toCSV(msg: SemanticdbFile): Product = {
  val path = msg.path
  val sdb = msg.sdb

  val occurrences = sdb.documents.map(x => x.occurrences.size).sum
  val synthetics = sdb.documents.map(x => x.synthetics.size).sum
  val symbols = sdb.documents.map(x => x.symbols.size).sum

  (sdb.documents.head.uri, occurrences, synthetics, symbols)
}

def run(mergedSemanticdbs: File): Unit = {
  for (input <- mergedSemanticdbs.newInputStream.autoClosed) {
    SemanticdbFile.streamFromDelimitedInput(input)
      .map(toCSV)
      .map(_.productIterator.mkString(","))
      .foreach(println)
  }
}

@main
def main(mergedSemanticdbs: String) = {
  run(File(mergedSemanticdbs))
}
