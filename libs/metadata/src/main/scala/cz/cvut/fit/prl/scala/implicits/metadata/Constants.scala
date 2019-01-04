package cz.cvut.fit.prl.scala.implicits.metadata

object Constants {
  val PathSep: String = System.getProperty("path.separator")
  val NL: String = System.getProperty("line.separator")
  val NA: String = "NA"
}

object MetadataFilenames {
  private val Prefix = "metadata"

  val AnalysisDirname = "_analysis_"
  val ProjectsDirname = "projects"

  val DependenciesFilename = s"$Prefix-dependencies.csv"
  val VersionsFilename = s"$Prefix-versions.csv"
  val SourcePathsFilename = s"$Prefix-sourcepaths.csv"
  val CleanPathsFilename = s"$Prefix-cleanpaths.csv"
}
