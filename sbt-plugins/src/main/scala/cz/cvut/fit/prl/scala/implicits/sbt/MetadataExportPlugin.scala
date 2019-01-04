package cz.cvut.fit.prl.scala.implicits.sbt
import java.io.FileWriter
import java.nio.file.Path

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

  private lazy val dependenciesFile =
    deleteIfExists(new File(analysisDir, InternalDependenciesFilename))

  private lazy val sourcepathsFile =
    deleteIfExists(new File(analysisDir, SourcePathsFilename))

  private lazy val versionsFile =
    deleteIfExists(new File(analysisDir, VersionsFilename))

  private lazy val classpathFile =
    deleteIfExists(new File(analysisDir, ExternalDependenciesFilename))

  private lazy val cleanpathFile =
    deleteIfExists(new File(analysisDir, CleanPathsFilename))

  override def globalSettings: Seq[Def.Setting[_]] = Seq(
    aggregate.in(MetadataKeys.allProjectsMetadata) := false,
    MetadataKeys.allProjectsMetadata := Def.taskDyn {
      val extracted = Project.extract(state.value)
      val refs = extracted.structure.allProjectRefs
      val filter = ScopeFilter(
        projects = inProjects(refs: _*),
        configurations = inConfigurations(Compile, Test))
      MetadataKeys.projectMetadata.all(filter)
    }.value,
    commands += Command.command("metadata") { state =>
      val extracted = Project.extract(state)
      val settings: Seq[Setting[_]] = for {
        p <- extracted.structure.allProjectRefs
        s <- List(
          sources.in(p).in(Compile) := Seq.empty,
          sources.in(p).in(Test) := Seq.empty
        )
      } yield s

      val installed = extracted.append(settings, state)
      "allProjectsMetadata" :: installed
    }
  )

  override lazy val projectSettings: Seq[Def.Setting[_]] =
    Seq(
      MetadataKeys.projectMetadata := {
        val forcedModuleName = moduleName.value
        val forcedOrganization = organization.value
        val forcedVersion = version.value

        println(s"** Processing: $forcedOrganization:$forcedModuleName:$forcedVersion in $repositoryRoot")

        val forcedScalaVersion = (scalaVersion in Compile).value
        val forcedSbtVersion = (sbtVersion in Compile).value
        val forcedCompileExternalDependencyClasspath =
          (externalDependencyClasspath in Compile).value
        val forcedTestExternalDependencyClasspath = (externalDependencyClasspath in Test).value
        val forcedCleanDirectories = (classDirectory in Compile).value :: (classDirectory in Test).value :: Nil

        val forcedCompileProjectDependencies =
          (projectDependencies in Compile).value
        val forcedTestProjectDependencies =
          (projectDependencies in Test).value
        val forcedCompileProductDirectories = (productDirectories in Compile).value
        val forcedTestProductDirectories = (productDirectories in Test).value

        val forcedManagedSourceDirectories =
          (managedSourceDirectories in Compile).value
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

        exportVersion(
          moduleId,
          forcedOrganization,
          forcedModuleName,
          forcedVersion,
          platform,
          forcedScalaVersion,
          forcedSbtVersion,
          updatedVersion,
          forcedCompileProductDirectories,
          forcedTestProductDirectories)

        exportSourcePaths(
          moduleId,
          forcedManagedSourceDirectories,
          forcedManagedTestDirectories,
          forcedUnmanagedSourceDirectories,
          forcedUnmanagedTestDirectories)

        exportInternalDependencies(
          moduleId,
          forcedCompileProjectDependencies,
          forcedTestProjectDependencies)

        exportExternalDependencies(
          moduleId,
          forcedCompileExternalDependencyClasspath,
          forcedTestExternalDependencyClasspath)

        exportCleanpaths(moduleId, forcedCleanDirectories)
      }
    )

  private def computeSLOC(path: Path): Try[Seq[SLOC]] = {
    if (path.toFile.exists()) {
      val cmd = s"cloc --quiet --csv $path"
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

  private def projectInternalDependency(projectId: String, moduleId: String, scope: String)(
      dependencyId: ModuleID): InternalDependency =
    InternalDependency(
      projectId,
      moduleId,
      dependencyId.organization,
      dependencyId.name,
      dependencyId.revision,
      scope
    )

  private def projectExternalDependency(projectId: String, moduleId: String, scope: String)(
      entry: Attributed[File]): ExternalDependency = {

    val path = entry.data.getAbsolutePath
    val (groupId, artifactId, version) = entry
      .get(moduleID.key)
      .map { module =>
        (module.organization, module.name, module.revision)
      }
      .getOrElse((NA, NA, NA))

    ExternalDependency(projectId, moduleId, groupId, artifactId, version, path, scope)
  }

  private def deleteIfExists(file: File): File = {
    if (file.exists()) file.delete()
    file
  }

  private def exportVersion(
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

    val version =
      Version(
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
        forcedCompileOutput.map(_.getAbsolutePath).mkString(PathSep),
        forcedTestOutput.map(_.getAbsolutePath).mkString(PathSep)
      )

    writeCSV(versionsFile, Version.CsvHeader, Seq(version))
  }

  private def exportInternalDependencies(
      moduleId: String,
      compileDependencies: Seq[ModuleID],
      testDependencies: Seq[ModuleID]): Unit = {

    val dependencies =
      compileDependencies.map(projectInternalDependency(projectId, moduleId, "compile")) ++
        testDependencies
          .diff(compileDependencies)
          .map(projectInternalDependency(projectId, moduleId, "test"))

    writeCSV(dependenciesFile, InternalDependency.CsvHeader, dependencies)
  }

  private def exportExternalDependencies(
      moduleId: String,
      compileDependencies: Classpath,
      testDependencies: Classpath): Unit = {

    val classpath =
      compileDependencies
        .map(projectExternalDependency(projectId, moduleId, "compile")) ++
        testDependencies
          .diff(compileDependencies)
          .map(projectExternalDependency(projectId, moduleId, "test"))

    writeCSV(classpathFile, ExternalDependency.CsvHeader, classpath)
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
      path <- paths.map(_.toPath.toAbsolutePath)
      slocs <- computeSLOC(path).toOption.toSeq
      sloc <- slocs
    } yield
      SourcePath(
        projectId,
        moduleId,
        scope,
        managed,
        repositoryRoot.relativize(path).toString,
        sloc)

    writeCSV(sourcepathsFile, SourcePath.CsvHeader, directories)
  }

  private def exportCleanpaths(moduleId: String, cleanDirectories: Seq[File]): Unit = {
    val cleanpaths = for (path <- cleanDirectories)
      yield CleanPath(projectId, moduleId, path.getAbsolutePath)

    writeCSV(cleanpathFile, CleanPath.CsvHeader, cleanpaths)
  }
}
