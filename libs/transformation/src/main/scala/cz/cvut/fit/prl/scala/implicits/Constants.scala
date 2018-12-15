package cz.cvut.fit.prl.scala.implicits
import cz.cvut.fit.prl.scala.implicits.utils.BuildInfo

object Constants {
  val ProjectsDirname = "projects"

  val AnalysisDirname = "_analysis_"
  val VersionsFilename = s"$AnalysisDirname/metadata-versions.csv"
  val ClasspathFilename = s"$AnalysisDirname/metadata-classpath.csv"
  val SourcepathFilename = s"$AnalysisDirname/metadata-sourcepath.csv"
  val MergedSemanticdbFilename = s"$AnalysisDirname/semanticdbs-${BuildInfo.scalametaVersion}.bin"
  val MergedSemanticdbStatsFilename = s"$AnalysisDirname/semanticdbs-${BuildInfo.scalametaVersion}.csv"

  val ExtractedImplicitsFilename = "implicits.bin"
  val ExtractionExceptionsFilename = "implicits-exceptions.csv"
  val ExtractionStatusFilename = "implicits-status.csv"

  lazy val ExcludedDirs: Seq[String] = Seq(".git", AnalysisDirname)


}
