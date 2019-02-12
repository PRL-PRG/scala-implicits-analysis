import $ivy.`cz.cvut.fit.prl.scala.implicits::tools:1.0-SNAPSHOT`
import $ivy.`cz.cvut.fit.prl.scala.implicits::metadata:1.0-SNAPSHOT`

import java.nio.file.FileVisitOption

import kantan.csv._
import kantan.csv.ops._
import kantan.csv.generic._

import better.files._
import cz.cvut.fit.prl.scala.implicits.model.Project
import cz.cvut.fit.prl.scala.implicits.metadata.MetadataFilenames._

val startingPath = File(".")
val projectsPath = startingPath / ProjectsDirname
val mergedImplicitsFile = startingPath / ExtractedImplicitsFilename

println(s"** MERGING implicits from: ${startingPath.path.toAbsolutePath}")

for {
  mergedOutput <- mergedImplicitsFile.newOutputStream.autoClosed
  projectPath <- projectsPath.list
} {
  val projectImplicits = projectPath / AnalysisDirname / ExtractedImplicitsFilename
  if (projectImplicits.exists) {
    println(s"Processing $projectImplicits ...")
    val implicits = projectImplicits.inputStream.apply(Project.streamFromDelimitedInput(_).toList)
    implicits.foreach(_.writeDelimitedTo(mergedOutput))
    println(s"Processed $projectImplicits")
  } else {
    println(s"Missing $projectImplicits")
  }
}
