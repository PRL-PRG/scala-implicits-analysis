import $ivy.`cz.cvut.fit.prl.scala.implicits::tools:1.0-SNAPSHOT`
import $ivy.`cz.cvut.fit.prl.scala.implicits::metadata:1.0-SNAPSHOT`

import better.files._
import cz.cvut.fit.prl.scala.implicits.model.Project
import cz.cvut.fit.prl.scala.implicits.metadata.MetadataFilenames._

def merge(projects: Traversable[File]): Unit = {
  val mergedImplicitsFile = File.currentWorkingDirectory / ExtractedImplicitsFilename

  println(s"** MERGING ${projects.size} implicits into: $mergedImplicitsFile")

  for {
    mergedOutput <- mergedImplicitsFile.newOutputStream.autoClosed
    project <- projects
  } {
    val projectImplicits = project / AnalysisDirname / ExtractedImplicitsFilename
    if (projectImplicits.exists) {
      println(s"Processing $projectImplicits ...")
      val implicits = projectImplicits.inputStream.apply(Project.streamFromDelimitedInput(_).toList)
      implicits.foreach(_.writeDelimitedTo(mergedOutput))
      println(s"Processed $projectImplicits")
    } else {
      println(s"Missing $projectImplicits")
    }
  }
}

@main
def main(projectFile: String, projectDir: String): Unit = {
  val projects = File(projectFile).lines.map(x => projectDir / x)
  merge(projects)
}