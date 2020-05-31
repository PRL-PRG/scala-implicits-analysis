import $ivy.`cz.cvut.fit.prl.scala.implicits:tools_2.12:1.0-SNAPSHOT`
import $ivy.`org.typelevel:kittens_2.12:1.2.0`

import better.files._
import cz.cvut.fit.prl.scala.implicits.model.{CallSite, ClasspathEntry, Declaration, Location, Module, PathEntry, Project, SourcepathEntry}
import cz.cvut.fit.prl.scala.implicits.model.Util._
import cz.cvut.fit.prl.scala.implicits.tools.ImplicitsPathAdjuster


@main
  def main(corporaDir: String = File.currentWorkingDirectory.toString, implicitsFileRelative: String = "/implicits.bin"): Unit = {

    val implicitsFile = File(corporaDir + implicitsFileRelative)
    val outputFile = File(corporaDir + "/implicits-valid.bin")

    val outputStream = outputFile.outputStream
    var projectCount: Int = 0
    var removedCount: Int = 0

    outputStream.apply(os => {
      implicitsFile.inputStream.apply(
        input =>
          Project.streamFrom(input).foreach(project => {
						val fixedPathsProject = ImplicitsPathAdjuster.fixProjectsPaths(project)
            projectCount += 1
            if (isValid(fixedPathsProject)) {
              Project.write(fixedPathsProject, os)
            } else {
              removedCount += 1
              println(s"Removed project: ${project.projectId}")
            }
          })
      )
    })
    println(s"Total projects: $projectCount, removed: $removedCount")
  }

  def isValid(project: Project): Boolean = {
    project.modules.values.forall(module => {
      module.declarations.values.forall(declaration => {
        module.paths.contains(declaration.location.path)
      })
    })
  }
