package cz.cvut.fit.prl.scala.implicits
import cz.cvut.fit.prl.scala.implicits.utils.BuildInfo.scalametaVersion
import cz.cvut.fit.prl.scala.implicits.metadata.MetadataFilenames.AnalysisDirname

object Constants {
  val MergedSemanticdbFilename = s"$AnalysisDirname/semanticdbs-$scalametaVersion.bin"
  val MergedSemanticdbStatsFilename = s"$AnalysisDirname/semanticdbs-$scalametaVersion.csv"

  val ExtractedImplicitsFilename = "implicits.bin"
  val ExtractionExceptionsFilename = "implicits-exceptions.csv"
  val ExtractionStatusFilename = "implicits-status.csv"

  lazy val ExcludedDirs: Seq[String] = Seq(".git", AnalysisDirname)


}
