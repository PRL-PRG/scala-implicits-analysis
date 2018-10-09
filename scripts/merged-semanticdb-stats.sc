import $ivy.`cz.cvut.fit.prl.scala.implicits::model:1.0-SNAPSHOT`
import $ivy.`cz.cvut.fit.prl.scala.implicits::transformation:1.0-SNAPSHOT`

import better.files._
import scala.meta.internal.semanticdb.TextDocuments

import $file.commons
import commons._

case class Stats(occurrences: Int, synthetics: Int, symbols: Int) {
  def +(that: Stats): Stats = Stats(occurrences + that.occurrences, synthetics + that.synthetics, symbols + that.symbols)

  def toCSV: String = productIterator.mkString(",")
}

def computeStats(sdb: TextDocuments): Stats = {
  val occurrences = sdb.documents.map(x => x.occurrences.size).sum
  val synthetics = sdb.documents.map(x => x.synthetics.size).sum
  val symbols = sdb.documents.map(x => x.symbols.size).sum

  Stats(occurrences, synthetics, symbols)
}

def run(pathsFile: File): Unit = {
  val projects = pathsFile.lines.toList

  projects.zipWithIndex.foreach { case (name, idx) =>
    val projectPath = File(s"$RunDir/$name")
    val inputFile = File(s"$projectPath/$OutputDir/$MergedSemanticdbsFilename")
    val outputFile = File(s"$projectPath/$OutputDir/$MergedSemanticdbsStatsFilename")

    print(s"Processing $name [${idx + 1}/${projects.size}] ")

    if (inputFile.exists) {
      val csv =
        inputFile
          .inputStream
          .apply(input => TextDocuments.streamFromDelimitedInput(input).map(computeStats).toList)
          .foldLeft(Stats(0, 0, 0))((a, b) => a + b)
          .toCSV

      outputFile.writeText(s"project,occurrencies,synthtics,symbols\n$name,$csv\n")

      println(s"done ($csv)")
    } else {
      println("skipped")
    }
  }
}

@main
def main(pathsFile: String) = {
  run(File(pathsFile))
}
