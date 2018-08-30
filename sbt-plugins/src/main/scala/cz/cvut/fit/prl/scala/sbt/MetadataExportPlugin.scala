package cz.cvut.fit.prl.scala.sbt

import java.io.FileWriter

import sbt.Keys._
import sbt._
import sbt.plugins.JvmPlugin

import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}

object MetadataExportPlugin extends AutoPlugin {
  val GHOrigin: Regex = "http[s]?://github.com/(.*)/(.*)(?:\\.git)?[\n]?".r

  case class SourceDir(projectId: String, projectName: String, scope: String, kind: String, path: String, sloc: SLOC)

  case class Version(projectId: String, projectName: String, scalaVersion: String, sbtVersion: String)

  case class SLOC(files: String, language: String, blank: String, comment: String, code: String) {
    override def toString = s"$files,$language,$blank,$comment,$code"
  }

  case class ProjectPath(projectId: String, projectName: String, path: String)

  object autoImport {
    val metadata = taskKey[Unit]("dumps project metadata")
  }

  import autoImport._

  override def trigger = allRequirements

  override def requires = JvmPlugin

  lazy val origin: String = {
    runCommand("git remote get-url origin").get
  }

  lazy val projectId: String = origin match {
    case GHOrigin(user, repo) => user + "--" + repo
    case _ => throw new Exception("Unable to get projectId")
  }

  lazy val analysisDir = {
    val tmp = new File("_analysis_")
    if (!tmp.exists()) {
      if (!tmp.mkdir()) throw new Exception("Unable to create " + tmp.getAbsolutePath)
    }

    tmp
  }

  lazy val sourceDirectoriesFile =
    deleteIfExists(new File(analysisDir, "source-directories.csv"))

  lazy val versionsFile =
    deleteIfExists(new File(analysisDir, "versions.csv"))

  lazy val classpathFile =
    deleteIfExists(new File(analysisDir, "classpath.csv"))

  lazy val cleanpathsFile =
    deleteIfExists(new File(analysisDir, "cleanpaths.csv"))

  override lazy val projectSettings = Seq(
    metadata := {
      val projectName = name.value
      println(s"Processing: $projectName")

      val sources = Seq(
        ("managed", "compile") -> (managedSourceDirectories in Compile).value,
        ("managed", "test") -> (managedSourceDirectories in Test).value,
        ("unmanaged", "compile") -> (unmanagedSourceDirectories in Compile).value,
        ("unmanaged", "test") -> (unmanagedSourceDirectories in Test).value
      )

      val directories = for {
        ((kind, scope), paths) <- sources
        path <- paths
        slocs <- computeSloc(path).toOption.toSeq
        sloc <- slocs
      } yield SourceDir(projectId, projectName, scope, kind, path.getAbsolutePath, sloc)

      writeCSV(
        sourceDirectoriesFile,
        "project_id,project_name,scope,kind,path,files,language,blank,comment,code",
        directories
      )

      val versions = Seq(
        Version(
          projectId,
          projectName,
          (scalaVersion in Compile).value,
          (sbtVersion in Compile).value
        )
      )

      writeCSV(versionsFile, "project_id,project_name,scala_version,sbt_version", versions)

      val classpath = for {
        path <- (fullClasspath in Test).value
      } yield ProjectPath(projectId, projectName, path.data.getAbsolutePath)

      writeCSV(classpathFile, "project_id,project_name,path", classpath)

      val cleanpaths = for (path <- cleanFiles.value) yield ProjectPath(projectId, projectName, path.getAbsolutePath)

      writeCSV(cleanpathsFile, "project_id,project_name,path", cleanpaths)
    }
  )

  def computeSloc(path: File): Try[Seq[SLOC]] = {
    if (path.exists()) {
      val cmd = s"cloc --quiet --csv ${path.getAbsolutePath}"

      runCommand(cmd) map { out =>
        // cloc CSV output starts with a new line followed by a header with a new line
        // we ignore both, getting right to the data
        out.split("\n").drop(2) map { line =>
          val x = line.split(",")
          SLOC(x(0), x(1), x(2), x(3), x(4))
        }
      }
    } else {
      Failure(new Exception(s"$path: directory does not exist"))
    }
  }

  def runCommand(cmd: String): Try[String] = {
    val stdout = new StringBuilder
    val stderr = new StringBuilder

    // we have to use FQN since sbt 0.13 also defines stringToProcess and ProcessLogger
    // both deprecated in sbt 1.0
    val status =
    sys.process.stringToProcess(cmd) ! sys.process.ProcessLogger(x => stdout append x + "\n", x => stderr append x + "\n")

    if (status == 0) {
      Success(stdout.toString())
    } else {
      Failure(new Exception(s"Command: $cmd failed with status $status\n$stderr"))
    }
  }

  def writeCSV(filename: File, header: String, data: Seq[Product], append: Boolean = true): Unit = {
    synchronized {
      val addHeader = !filename.exists()
      var w: FileWriter = null
      try {
        w = new FileWriter(filename, append)

        if (addHeader) {
          w.write(header + "\n")
        }

        w.write(data.map(_.productIterator.mkString(",")).mkString("\n"))

        if (data.nonEmpty) {
          w.write("\n")
        }

      } finally {
        if (w != null) {
          w.close()
        }
      }
    }
  }

  def deleteIfExists(x: File): File = {
    if (x.exists()) {
      x.delete()
    }

    x
  }
}
