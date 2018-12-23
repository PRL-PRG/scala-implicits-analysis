package cz.cvut.fit.prl.scala.implicits
import cz.cvut.fit.prl.scala.implicits.utils.BuildInfo.scalametaVersion
import cz.cvut.fit.prl.scala.implicits.metadata.MetadataFilenames.AnalysisDirname

object Constants {
  val MergedSemanticdbFilename = s"$AnalysisDirname/semanticdb-$scalametaVersion.bin"
  val MergedSemanticdbStatsFilename = s"$AnalysisDirname/semanticdb-stats-$scalametaVersion.csv"

  val ExtractedImplicitsFilename = "implicits.bin"
  val ExtractionExceptionsFilename = "implicits-exceptions.csv"
  val ExtractionStatsFilename = "implicits-stats.csv"

  val ExcludedDirs: Seq[String] = Seq(".git", AnalysisDirname)
}
