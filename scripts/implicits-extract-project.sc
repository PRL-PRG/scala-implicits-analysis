import $ivy.`cz.cvut.fit.prl.scala.implicits:tools_2.12:1.0-SNAPSHOT`
import $ivy.`org.typelevel:kittens_2.12:1.2.0`

import better.files._
import cz.cvut.fit.prl.scala.implicits.model.Project
import cz.cvut.fit.prl.scala.implicits.model.Util._


@main
def main(projectId: String, implicitsFilePath: String = File.currentWorkingDirectory.toString + "/implicits2.bin", outputFileName: String = ""): Unit = {

  val outputName: String = if (outputFileName.isEmpty) s"extracted-implicit-project-$projectId" else outputFileName
  val implicitsFile = File(implicitsFilePath)
  val outputFile = File(outputName)

  val outputStream = outputFile.outputStream

  outputStream.apply(os => {
    implicitsFile.inputStream.apply(
      input =>
        Project.streamFrom(input).find(project => project.projectId == projectId) match {
          case Some(project: Project) => Project.write(project, os)
          case None => println(s"Project with projectId: $projectId, not found")
        }
    )
  })
}
