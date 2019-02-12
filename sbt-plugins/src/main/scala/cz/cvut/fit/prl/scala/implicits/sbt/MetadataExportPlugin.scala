package cz.cvut.fit.prl.scala.implicits.sbt

import java.io.FileWriter
import java.nio.file.Path
import java.io.File

import cz.cvut.fit.prl.scala.implicits.metadata.Constants.{NA, NL, PathSep}
import cz.cvut.fit.prl.scala.implicits.metadata.MetadataFilenames._
import cz.cvut.fit.prl.scala.implicits.metadata._
import sbt.Keys._
import sbt._
import sbt.plugins.JvmPlugin

import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}

object MetadataExportPlugin extends AutoPlugin {
  object MetadataKeys {
    val allProjectsMetadata = taskKey[Unit]("dumps all projects metadata")
    val projectMetadata = taskKey[Unit]("dumps project metadata")
  }

  override def trigger = allRequirements
  override def requires = JvmPlugin

  final val autoImport = MetadataKeys

  private val GHOrigin: Regex =
    "^http[s]?://github.com/([^/]+)/(.*)$".r

  private lazy val origin: String = {
    runCommand("git remote get-url origin").get.trim
  }

  private lazy val commit: String = {
    runCommand("git log --pretty=format:%h -n 1").get.trim
  }

  private lazy val repositoryRoot: Path = {
    //runCommand("git rev-parse --show-toplevel").get.trim
    java.nio.file.Paths.get("").toAbsolutePath
  }

  private def relativize(x: File): Path =
    repositoryRoot.relativize(x.toPath)

  private lazy val projectId: String = origin match {
    case GHOrigin(user, repo) => user + "--" + repo.replaceAll("\\.git$", "")
    case _ => throw new Exception("Unable to get projectId")
  }

  private lazy val analysisDir = {
    val tmp = new File(AnalysisDirname)
    if (!tmp.exists()) {
      if (!tmp.mkdir())
        throw new Exception("Unable to create " + tmp.getAbsolutePath)
    }

    tmp
  }

  private lazy val sourcepathsFile =
    deleteIfExists(new File(analysisDir, SourcePathsFilename))

  private lazy val modulesFile =
    deleteIfExists(new File(analysisDir, ModulesFilename))

  private lazy val classpathFile =
    deleteIfExists(new File(analysisDir, DependenciesFilename))

  private lazy val cleanpathFile =
    deleteIfExists(new File(analysisDir, CleanPathsFilename))

  override def globalSettings: Seq[Def.Setting[_]] = Seq(
    aggregate.in(MetadataKeys.allProjectsMetadata) := false,
    MetadataKeys.allProjectsMetadata := Def.taskDyn {
      val extracted = Project.extract(state.value)
      val currentProjectRef = extracted.currentRef
      val filter = ScopeFilter(
        projects = inDependencies(currentProjectRef, transitive = true, includeRoot = true) ||
          inAggregates(currentProjectRef, transitive = true, includeRoot = true),
        configurations = inConfigurations(Compile, Test))
      MetadataKeys.projectMetadata.all(filter)
    }.value,
    commands += Command.command("metadata") { state =>
      // for example in scala--scala, asking for dependencyClasspath in Test
      // triggers running scala.tools.docutil.ManMaker which however
      // is not compiled yet as the manual project will not be scheduled in
      // neither inAggregate aor inDependencies
      "set every resourceGenerators := Seq.empty" ::
        "set every sources := Seq.empty" ::
        "allProjectsMetadata" ::
        state
    }
  )

  override lazy val projectSettings: Seq[Def.Setting[_]] =
    Seq(
      MetadataKeys.projectMetadata := {
        val forcedModuleName = moduleName.value
        val forcedOrganization = organization.value
        val forcedVersion = version.value

        println(
          s"** Processing: $forcedOrganization:$forcedModuleName:$forcedVersion in $repositoryRoot")

        val forcedScalaVersion = (scalaVersion in Compile).value
        val forcedSbtVersion = (sbtVersion in Compile).value
        val forcedCompileDependencyClasspath = (dependencyClasspath in Compile).value
        val forcedTestDependencyClasspath = (dependencyClasspath in Test).value
        val forcedCleanDirectories = (classDirectory in Compile).value :: (classDirectory in Test).value :: Nil

        val forcedCompileProductDirectories = (productDirectories in Compile).value
        val forcedTestProductDirectories = (productDirectories in Test).value

        val forcedManagedSourceDirectories: Seq[File] = {
          val managed = (managedSourceDirectories in Compile).value
          val base = baseDirectory.value
          val forcedSourcesInBase = sourcesInBase.value

          // sometimes the sources are not in what is reported by managedSourcesDirectories, but
          // are in the baseDirectory (cf. scala--pickle/banechmark)
          // this is reported by sourcesInBase, but that turned out to be true all the time
          // this is problematic for the root aggregating projects

          val managedFiles =
            managed.foldLeft(0)((a, b) => a + Option(b.list()).map(_.length).getOrElse(0))

          val baseFiles = Option(base.listFiles())
            .map(_.count(x => x.isFile && x.getName.endsWith(".scala")))
            .getOrElse(0)

          if (managedFiles == 0 && forcedSourcesInBase && baseFiles > 0) {
            // no files in any of the managedSourcesDirectories so we add base
            // this is for the simple sbt projects like:
            //   - project/build.sbt
            //   - project/A.scala
            Seq(base)
          } else {
            if (forcedSourcesInBase && baseFiles > 0 && (base.getAbsolutePath != repositoryRoot.toFile.getAbsolutePath)) {
              managed :+ base
            } else {
              managed
            }
          }
        }

        val forcedManagedTestDirectories =
          (managedSourceDirectories in Test).value
        val forcedUnmanagedSourceDirectories =
          (unmanagedSourceDirectories in Compile).value
        val forcedUnmanagedTestDirectories =
          (unmanagedSourceDirectories in Test).value

        val updatedVersion = CrossVersion
          .partialVersion(forcedScalaVersion)
          .map { case (maj, min) => Config.updateVersion(maj, min, forcedScalaVersion) }
          .getOrElse("NA")

        val (updatedCompileProductDirectories, updatedTestProductDirectories) =
          (
            CrossVersion.partialVersion(forcedScalaVersion),
            CrossVersion.partialVersion(updatedVersion)
          ) match {
            case (Some((_, from)), Some((_, to))) if from != to =>
              (
                forcedCompileProductDirectories.map(x =>
                  replaceScalaMinorVersionInPath(x, from, to)),
                forcedTestProductDirectories.map(x => replaceScalaMinorVersionInPath(x, from, to))
              )
            case _ =>
              (forcedCompileProductDirectories, forcedTestProductDirectories)
          }

        // we need to check if the project might build for multiple platforms
        // a better way would be to somehow ask the
        // crossplatform plugin (https://github.com/portable-scala/sbt-crossproject), but
        // the following should do
        val forcedLibraryDependencies = libraryDependencies.value
        val platform = {
          val candidates = forcedLibraryDependencies.collect { case m => m.organization -> m.name }

          if (Config.ScalaNativeLibs.forall(candidates.contains)) "native"
          else if (Config.ScalaJsLibs.forall(candidates.contains)) "js"
          else "jvm"
        }

        val moduleId =
          MetadataUtils.moduleId(forcedOrganization, forcedModuleName, forcedVersion, platform)

        println(s"** ModuleID: $moduleId")

        exportModules(
          moduleId,
          forcedOrganization,
          forcedModuleName,
          forcedVersion,
          platform,
          forcedScalaVersion,
          forcedSbtVersion,
          updatedVersion,
          updatedCompileProductDirectories,
          updatedTestProductDirectories)

        exportSourcePaths(
          moduleId,
          forcedManagedSourceDirectories,
          forcedManagedTestDirectories,
          forcedUnmanagedSourceDirectories,
          forcedUnmanagedTestDirectories)

        exportDependencyClasspath(
          moduleId,
          forcedCompileDependencyClasspath,
          forcedTestDependencyClasspath)

        exportCleanpaths(moduleId, forcedCleanDirectories)
      }
    )

  private def replaceScalaMinorVersionInPath(in: File, from: Long, to: Long): File =
    replaceScalaMinorVersionInPath(in, from.toInt, to.toInt)

  private def replaceScalaMinorVersionInPath(in: File, from: Int, to: Int): File =
    new File(
      in.getAbsolutePath.replace(
        s"target/scala-2.$from/",
        s"target/scala-2.$to/"
      )
    )

  private def computeSLOC(path: File): Try[Seq[SLOC]] = {
    if (path.isDirectory) {
      val cmd = s"cloc --include-lang=Scala --quiet --csv ${path.getAbsolutePath}"
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

  private def runCommand(cmd: String): Try[String] = {
    val stdout = new StringBuilder
    val stderr = new StringBuilder

    // we have to use FQN since sbt 0.13 also defines stringToProcess and ProcessLogger
    // both deprecated in sbt 1.0
    val exitcode = sys.process.stringToProcess(cmd) ! sys.process.ProcessLogger(
      x => stdout append x + NL,
      x => stderr append x + NL
    )

    val status = if (exitcode == 0) {
      Success(stdout.toString().trim)
    } else {
      Failure(new Exception(s"Command: $cmd failed with status $exitcode\n$stderr"))
    }

    status
  }

  private def writeCSV(
      filename: File,
      columnNames: Seq[String],
      data: Seq[Product],
      append: Boolean = true): Unit = {

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

  private def projectDependency(projectId: String, moduleId: String, scope: String)(
      entry: Attributed[File]): Dependency = {

    val path = relativize(entry.data).toString
    val (groupId, artifactId, version, transitive) = entry
      .get(moduleID.key)
      .map { module =>
        (module.organization, module.name, module.revision, module.isTransitive)
      }
      .getOrElse((NA, NA, NA, false))

    Dependency(projectId, moduleId, groupId, artifactId, version, path, scope, transitive)
  }

  private def deleteIfExists(file: File): File = {
    if (file.exists()) file.delete()
    file
  }

  private def exportModules(
      moduleId: String,
      projectOrganization: String,
      projectName: String,
      projectVersion: String,
      platform: String,
      forcedScalaVersion: String,
      forcedSbtVersion: String,
      updatedScalaVersion: String,
      forcedCompileOutput: Seq[File],
      forcedTestOutput: Seq[File]): Unit = {

    val module =
      Module(
        projectId,
        moduleId,
        projectOrganization,
        projectName,
        projectVersion,
        platform,
        commit,
        forcedScalaVersion,
        forcedSbtVersion,
        updatedScalaVersion,
        forcedCompileOutput.map(relativize).mkString(PathSep),
        forcedTestOutput.map(relativize).mkString(PathSep)
      )

    writeCSV(modulesFile, Module.CsvHeader, Seq(module))
  }

  private def exportDependencyClasspath(
      moduleId: String,
      compileDependencies: Classpath,
      testDependencies: Classpath): Unit = {

    val classpath =
      compileDependencies
          .filter(_.data.isAbsolute)
          .map(projectDependency(projectId, moduleId, "compile")) ++
        testDependencies
          .filter(_.data.isAbsolute)
          .map(projectDependency(projectId, moduleId, "test"))

    (compileDependencies ++ testDependencies)
      .filterNot(_.data.isAbsolute)
      .foreach(x => println(s"*** Non-absolute path for $moduleId: ${x.data} ($x)"))

    writeCSV(classpathFile, Dependency.CsvHeader, classpath.distinct)
  }

  private def exportSourcePaths(
      moduleId: String,
      managedSourceDirectories: Seq[File],
      managedTestDirectories: Seq[File],
      unmanagedSourceDirectories: Seq[File],
      unmanagedTestDirectories: Seq[File]): Unit = {

    val sources = Seq(
      (true, "compile", managedSourceDirectories),
      (true, "test", managedTestDirectories),
      (false, "compile", unmanagedSourceDirectories),
      (false, "test", unmanagedTestDirectories)
    )

    val directories = for {
      (managed, scope, paths) <- sources
      path <- paths
      slocs <- computeSLOC(path).toOption.toSeq
      sloc <- slocs
    } yield
      SourcePath(
        projectId,
        moduleId,
        scope,
        managed,
        relativize(path).toString,
        sloc)

    writeCSV(sourcepathsFile, SourcePath.CsvHeader, directories)
  }

  private def exportCleanpaths(moduleId: String, cleanDirectories: Seq[File]): Unit = {
    val cleanpaths = for (path <- cleanDirectories)
      yield CleanPath(projectId, moduleId, relativize(path).toString)

    writeCSV(cleanpathFile, CleanPath.CsvHeader, cleanpaths)
  }
}
