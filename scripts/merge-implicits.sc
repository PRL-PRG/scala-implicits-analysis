import $ivy.`cz.cvut.fit.prl.scala.implicits::tools:1.0-SNAPSHOT`
import $ivy.`cz.cvut.fit.prl.scala.implicits::metadata:1.0-SNAPSHOT`

import better.files._
import cz.cvut.fit.prl.scala.implicits.model.Project
import cz.cvut.fit.prl.scala.implicits.model.Util._
import cz.cvut.fit.prl.scala.implicits.metadata.MetadataFilenames._

import java.io.OutputStream

import cats.Monoid

import scala.util.{Try, Success, Failure}

case class Stats(
      projects: Int,
      bytes: Long,
      missing: List[String],
      failed: List[(String, Throwable)]) {
  def prettyPrint(): Unit = {
    println(s"Projects $projects ($bytes bytes)")
    println(s"Missing projects: ${missing.size}")
    if (failed.nonEmpty) {
      println(s"Failed projects: ${failed.size}")
      println(failed.map(x => x._1 + ": " + x._2.getMessage).mkString("\t", "\n\t", "\n"))
    }
  }
}

implicit val monoidStats: Monoid[Stats] = new Monoid[Stats] {
    override def empty = Stats(0, 0, Nil, Nil)
    override def combine(x: Stats, y: Stats) =
      Stats(
        x.projects + y.projects,
        x.bytes + y.bytes,
        x.missing ++ y.missing,
        x.failed ++ y.failed
      )
  }

def mergeOne(projectId: String, dataFile: File, output: OutputStream): Stats = {
    if (dataFile.exists) {
      Try {
        val project = dataFile.inputStream.apply(Project.parseFrom)
        val size = Project.write(project, output)

        Stats(1, size, Nil, Nil)
      } match {
        case Success(stats) =>
          println(s"Written ${stats.bytes} bytes for $projectId")
          stats
        case Failure(e) =>
          println(s"Failed $dataFile")
          Stats(0, 0, Nil, projectId -> e :: Nil)
      }
    } else {
      println(s"Missing $dataFile")
      Stats(0, 0, projectId :: Nil, Nil)
    }
  }

def merge(projectDirs: Iterator[File]): Stats = {
    val outputFile = File(ExtractedImplicitsFilename)

    println(s"** MERGING into: $outputFile")

    val pipe = for {
      output <- outputFile.outputStream
      (projectDir, idx) <- projectDirs.zipWithIndex
    } yield {
      val projectId = projectDir.name
      val dataFile = projectDir / AnalysisDirname / ExtractedImplicitsFilename

      println(s"Processing ($idx) $dataFile")

      mergeOne(projectId, dataFile, output)
    }

    Monoid[Stats].combineAll(pipe)
  }

def reread(): Stats = {
    val inputFile = File(ExtractedImplicitsFilename)

    val pipe = for {
      input <- inputFile.inputStream
      (project, idx) <- Project.streamFrom(input).zipWithIndex
    } yield {
      println(s"Read ($idx) ${project.projectId}")
      Stats(1, 0, Nil, Nil)
    }

    Monoid[Stats].combineAll(pipe)
  }

@main
  def main(projectFile: String, projectDir: String): Unit = {
    val projects = File(projectFile).lineIterator.map(x => projectDir / x)
    val writeStats = Try(merge(projects))
    val readStats = Try(reread())

    for {
      ws <- writeStats
      rs <- readStats
    } {
      println("Write stats:")
      ws.prettyPrint()

      println("Read stats:")
      rs.prettyPrint()

      if (rs.projects != ws.projects) {
        println(s"*** CHECKSUM FAILED: missing ${ws.projects - rs.projects} !!! ***")
      }
    }
  }