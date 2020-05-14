import $ivy.`cz.cvut.fit.prl.scala.implicits:tools_2.12:1.0-SNAPSHOT`
import $ivy.`org.typelevel:kittens_2.12:1.2.0`

import better.files._
import cz.cvut.fit.prl.scala.implicits.model.{CallSite, ClasspathEntry, Declaration, Location, Module, PathEntry, Project, SourcepathEntry}
import cz.cvut.fit.prl.scala.implicits.model.Util._


@main
  def main(corporaDir: String = File.currentWorkingDirectory.toString, implicitsFileRelative: String = "/implicits-valid.bin", extractionFactor: Int = 100): Unit = {

    val implicitsFile = File(corporaDir + implicitsFileRelative)
    val outputFile = File(corporaDir + s"/implicits-extracted-$extractionFactor.bin")

    val outputStream = outputFile.outputStream
    var projectCount: Int = 0
    var savedCount: Int = 0

    outputStream.apply(os => {
      implicitsFile.inputStream.apply(
        input =>
          Project.streamFrom(input).foreach(project => {
            if (projectCount % extractionFactor == 0) {
              Project.write(project, os)
              savedCount += 1
            }
            projectCount += 1
          })
      )
    })
    println(s"Total projects: $projectCount, saved: $savedCount")
  }

