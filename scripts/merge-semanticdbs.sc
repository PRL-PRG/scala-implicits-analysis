import $ivy.`cz.cvut.fit.prl.scala.implicits::tools:1.0-SNAPSHOT`
import $ivy.`cz.cvut.fit.prl.scala.implicits::metadata:1.0-SNAPSHOT`

import java.nio.file.FileVisitOption

import kantan.csv._
import kantan.csv.ops._
import kantan.csv.generic._

import better.files._
import cz.cvut.fit.prl.scala.implicits.metadata.MetadataFilenames._
import cz.cvut.fit.prl.scala.implicits.utils.SdbLocator

val startingPath = File.currentWorkingDirectory
val semanticdbMergedFile = startingPath / AnalysisDirname / SemanticdbMergedFilename
val semanticdbMergedStatsFile = startingPath / AnalysisDirname / SemanticdbMergedStatsFilename

println(s"** MERGING semanticdbs from: ${startingPath.path.toAbsolutePath}")

var stats = (0, 0, 0, 0)

for {
  mergedOutput <- semanticdbMergedFile.newOutputStream.autoClosed
} {
  new SdbLocator(startingPath.path)
      .exclude(ExcludedDirs)
      .options(FileVisitOption.FOLLOW_LINKS)
      .run {
        case (_, db) =>
          stats = (
            stats._1 + 1,
            stats._2 + db.documents.map(_.occurrences.size).sum,
            stats._3 + db.documents.map(_.synthetics.size).sum,
            stats._4 + db.documents.map(_.symbols.size).sum
          )

          db.documents.foreach { d =>
            d.writeDelimitedTo(mergedOutput)
          }
      }
}

for {
  output <- semanticdbMergedStatsFile
    .toJava
    .asCsvWriter[(Int, Int, Int, Int)](rfc.withHeader("files", "occurrences", "synthetics", "symbols")).autoClosed
} output.write(stats)

println(s"** MERGED $stats into $semanticdbMergedFile")
