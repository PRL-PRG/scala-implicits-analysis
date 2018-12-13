package cz.cvut.fit.prl.scala.sbt

import java.io.FileWriter

import sbt.Keys._
import sbt._

import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}

object MetadataExportPlugin extends AutoPlugin {
  val GHOrigin: Regex = "^http[s]?://github.com/([^/]+)/([^.]+)(\\.git)?$".r

  case class SLOC(files: String, language: String, blank: String, comment: String, code: String) {
    override def toString = s"$files,$language,$blank,$comment,$code"
  }

  case class SourceDir(projectId: String, projectName: String, scope: String, managed: Boolean, path: String, sloc: SLOC)

  case class Version(projectId: String, projectName: String, commit: String, scalaVersion: String, sbtVersion: String)

  case class ProjectClasspath(projectId: String, projectName: String, path: String, scope: String)

  case class ProjectPath(projectId: String, projectName: String, path: String)

  object autoImport {
    val metadata = taskKey[Unit]("dumps project metadata")
  }

  import autoImport._

  override def trigger = allRequirements

  override def requires = empty

  lazy val origin: String = {
    runCommand("git remote get-url origin").get.trim
  }

  lazy val commit: String = {
    runCommand("git log --pretty=format:%h -n 1").get.trim
  }

  lazy val projectId: String = origin match {
    case GHOrigin(user, repo, _) => user + "--" + repo
    case _ => throw new Exception("Unable to get projectId")
  }

  lazy val analysisDir = {
    val tmp = new File(Config.AnalysisDir)
    if (!tmp.exists()) {
      if (!tmp.mkdir()) throw new Exception("Unable to create " + tmp.getAbsolutePath)
    }

    tmp
  }

  val sourceDirectoriesFile =
    new File(analysisDir, "metadata-sourcepath.csv")

  val versionsFile =
    new File(analysisDir, "metadata-versions.csv")

  val classpathFile =
    new File(analysisDir, "metadata-classpath.csv")

  val cleanpathFile =
    new File(analysisDir, "metadata-cleanpath.csv")

  override lazy val projectSettings = Seq(
    metadata := {
      val projectName = name.value
      println(s"Processing: $projectName")

      // we need to force this here so it does not complain that it is in the loop
      val forcedScalaVersion = (scalaVersion in Compile).value
      val forcedSbtVersion = (sbtVersion in Compile).value
      val forcedClasspathCompile = (fullClasspath in Compile).value
      val forcedClasspathTest = (fullClasspath in Test).value
      val forcedCleanFiles = cleanFiles.value

      val sources = Seq(
        (true, "compile") -> (managedSourceDirectories in Compile).value,
        (true, "test") -> (managedSourceDirectories in Test).value,
        (false, "compile") -> (unmanagedSourceDirectories in Compile).value,
        (false, "test") -> (unmanagedSourceDirectories in Test).value
      )

      val directories = for {
        ((kind, scope), paths) <- sources
        path <- paths
        slocs <- computeSLOC(path).toOption.toSeq
        sloc <- slocs
      } yield SourceDir(projectId, projectName, scope, kind, path.getAbsolutePath, sloc)

      writeCSV(
        sourceDirectoriesFile,
        Seq("project_id", "project_name", "scope", "kind", "path", "files", "language", "blank", "comment", "code"),
        directories
      )

      val versions = Seq(
        Version(
          projectId,
          projectName,
          commit,
          forcedScalaVersion,
          forcedSbtVersion
        )
      )

      writeCSV(versionsFile, Seq("project_id", "project_name", "commit", "scala_version", "sbt_version"), versions)


      val classpath =
        forcedClasspathCompile
          .map(x => ProjectClasspath(projectId, projectName, x.data.getAbsolutePath, "compile")) ++
          forcedClasspathTest
            .diff(forcedClasspathCompile)
            .map(x => ProjectClasspath(projectId, projectName, x.data.getAbsolutePath, "test"))

      writeCSV(classpathFile, Seq("project_id", "project_name", "path", "scope"), classpath)

      val cleanpaths = for (path <- forcedCleanFiles) yield ProjectPath(projectId, projectName, path.getAbsolutePath)

      writeCSV(cleanpathFile, Seq("project_id", "project_name", "path"), cleanpaths)
    }
  )

  private val NL = System.getProperty("line.separator")

  def computeSLOC(path: File): Try[Seq[SLOC]] = {
    if (path.exists()) {
      val cmd = s"cloc --quiet --csv ${path.getAbsolutePath}"
      val output = runCommand(cmd)

      output map { out =>
        // cloc CSV output starts with a header with a new line
        out.split(NL).drop(1) map { line =>
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
    val exitcode =
    sys.process.stringToProcess(cmd) ! sys.process.ProcessLogger(
      x => stdout append x + NL,
      x => stderr append x + NL
    )

    val status =
      if (exitcode == 0) {
        Success(stdout.toString().trim)
      } else {
        Failure(new Exception(s"Command: $cmd failed with status $exitcode\n$stderr"))
      }

    status
  }

  def writeCSV(filename: File, columnNames: Seq[String], data: Seq[Product], append: Boolean = true): Unit = {
    def escape(x: Any): Any = x match {
      case y: String => '"' + y + '"'
      case y => y
    }

    synchronized {
      val addHeader = !filename.exists()
      var w: FileWriter = null
      try {
        w = new FileWriter(filename, append)

        if (addHeader) {
          w.write(columnNames.mkString(",") + NL)
        }

        w.write(data.map(_.productIterator.map(escape).mkString(",")).mkString(NL))

        if (data.nonEmpty) {
          w.write(NL)
        }

      } finally {
        if (w != null) {
          w.close()
        }
      }
    }
  }
}
