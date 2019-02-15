package cz.cvut.fit.prl.scala.implicits.metadata

object MetadataFilenames {
  private val Prefix = "metadata"

  val AnalysisDirname = "_analysis_"
  val ProjectsDirname = "projects"

  val DependenciesFilename = s"$Prefix-dependencies.csv"
  val ModulesFilename = s"$Prefix-modules.csv"
  val SourcePathsFilename = s"$Prefix-sourcepaths.csv"
  val CleanPathsFilename = s"$Prefix-cleanpaths.csv"

  val ExtractedImplicitsFilename = "implicits.bin"
  val ExtractionExceptionsFilename = "implicits-exceptions.csv"
  val ExtractionStatsFilename = "implicits-stats.csv"

  val SemanticdbMergedFilename = s"semanticdb.bin"
  val SemanticdbMergedStatsFilename = s"semanticdb-stats.csv"

  val ExcludedDirs: Seq[String] = Seq(
    AnalysisDirname,
    "_coursier_",
    ".git",
    "_ivy_",
    "_sbt_"
  )
}
