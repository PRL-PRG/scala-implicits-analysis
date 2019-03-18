import $ivy.`cz.cvut.fit.prl.scala.implicits::tools:1.0-SNAPSHOT`
import $ivy.`cz.cvut.fit.prl.scala.implicits::metadata:1.0-SNAPSHOT`

import better.files._
import cz.cvut.fit.prl.scala.implicits.metadata.MetadataFilenames._
import cz.cvut.fit.prl.scala.implicits.model._
import cz.cvut.fit.prl.scala.implicits.model.Util._
import cz.cvut.fit.prl.scala.implicits.tools._

import org.slf4j.LoggerFactory
import ch.qos.logback.classic.Level
import ch.qos.logback.classic.Logger

@main
  def main() = {
    val baseDir = File.currentWorkingDirectory
    val projectsFile = baseDir / ExtractedImplicitsFilename

    val root = LoggerFactory.getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME).asInstanceOf[Logger]
    root.setLevel(Level.INFO)

    val startTime = System.currentTimeMillis()
    for {
      input <- projectsFile.inputStream
      cse <- CallSitesExporter.createExporter().autoClosed
      dse <- DeclarationsExporter.createExporter().autoClosed
      exporters = Seq(cse, dse)
      (project, idx) <- Project.streamFrom(input).zipWithIndex
    } {
      println(
        s"[${System.currentTimeMillis() - startTime}] Exporting ${idx + 1} project ${project.projectId}")
      exporters.foreach(_(project))
    }
  }
