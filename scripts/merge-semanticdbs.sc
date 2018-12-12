import $ivy.`cz.cvut.fit.prl.scala.implicits::transformation:1.0-SNAPSHOT`
import java.nio.file.FileVisitOption

import better.files._
import cz.cvut.fit.prl.scala.implicits.Constants._
import cz.cvut.fit.prl.scala.implicits.utils.SdbLocator

val startingPath = File(".")
val mergedSemanticdbFile = startingPath / MergedSemanticdbFilename
val mergedSemanticdbStatsFile = startingPath / MergedSemanticdbStatsFilename

println(s"** MERGING from: ${startingPath.path.toAbsolutePath}")

var occurences = 0
var synthetics = 0
var symbols = 0

mergedSemanticdbFile.outputStream.apply { output =>
    new SdbLocator(startingPath.path)
      .exclude(ExcludedDirs)
      .options(FileVisitOption.FOLLOW_LINKS)
      .run {
        case (_, db) => {
          occurences += db.documents.map(_.occurrences.size).sum
          synthetics += db.documents.map(_.synthetics.size).sum
          symbols += db.documents.map(_.symbols.size).sum

          db.documents.foreach(_.writeDelimitedTo(output))
        }
      }
  }

val stats =
    s"""
     |occurrences,synthetics,symbols
     |$occurences,$synthetics,$symbols
     |""".stripMargin

mergedSemanticdbStatsFile.writeText(stats)

println(s"** MERGED $occurences occurences, $synthetics synthetics, $symbols symbols into $mergedSemanticdbFile")
