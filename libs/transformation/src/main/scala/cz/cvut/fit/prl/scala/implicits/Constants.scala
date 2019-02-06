package cz.cvut.fit.prl.scala.implicits
import cz.cvut.fit.prl.scala.implicits.utils.BuildInfo.scalametaVersion
import cz.cvut.fit.prl.scala.implicits.metadata.MetadataFilenames.AnalysisDirname

object Constants {
  // TODO: remove the scalameta version number
  // TODO: move to metadata
  val MergedSemanticdbFilename = s"$AnalysisDirname/semanticdb-$scalametaVersion.bin"
  val MergedSemanticdbStatsFilename = s"$AnalysisDirname/semanticdb-stats-$scalametaVersion.csv"

  val ExcludedDirs: Seq[String] = Seq(".git", AnalysisDirname)
}
