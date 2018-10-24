package cz.cvut.fit.prl.scala.implicits

object Constants {
  lazy val ExcludedDirs: Seq[String] = Seq(".git", AnalysisDirname)
  val ExtractedImplicitsFilename = "implicits.bin"
  val ProjectsDirname = "projects"
  val AnalysisDirname = "_analysis_"
  val MergedSemanticdbFilename = "semanticdbs-4.0.0.bin"
  val MergedSemanticdbStatsFilename = "semanticdbs-4.0.0.csv"
}
