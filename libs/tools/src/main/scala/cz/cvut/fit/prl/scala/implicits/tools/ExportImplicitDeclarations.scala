package cz.cvut.fit.prl.scala.implicits.tools

import better.files._
import com.typesafe.scalalogging.Logger
import cz.cvut.fit.prl.scala.implicits.model._
import cz.cvut.fit.prl.scala.implicits.model.Util.timedTask
import cz.cvut.fit.prl.scala.implicits.utils.BuildInfo
import kantan.csv._
import kantan.csv.ops._
import kantan.csv.generic._
import org.slf4j.LoggerFactory

object ExportImplicitDeclarations extends App {

  implicit val logger = Logger(LoggerFactory.getLogger(getClass.getName))

  case class Output(
      projectId: String,
      groupId: String,
      artifactId: String,
      version: String,
      kind: String,
      fqn: String,
      name: String,
      locationPath: String,
      locationUri: String,
      locationPos: String,
      defGroupId: String,
      defArtifactId: String,
      defVersion: String,
      isCompanion: Boolean,
      numTypeParameters: Int,
      numParameterLists: Int,
      numParameters: Int,
      numImplicitParameters: Int
  )

  object Output {
    val Header = Seq(
      "project_id",
      "group_id",
      "artifact_id",
      "version",
      "kind",
      "fqn",
      "name",
      "location_path",
      "location_uri",
      "location_pos",
      "def_group_id",
      "def_artifact_id",
      "def_version",
      "is_companion",
      "num_type_parameters",
      "num_parameter_lists",
      "num_parameters",
      "num_implicit_parameters"
    )

    def apply(declaration: Declaration)(implicit idx: Index): Output = {
      val project = declaration.project
      val module = declaration.module
      val library = declaration.library

      Output(
        projectId = project.projectId,
        groupId = module.groupId,
        artifactId = module.artifactId,
        version = module.version,
        kind = declaration.kind.name,
        fqn = declaration.declarationId,
        name = declaration.name,
        locationPath = declaration.location.path,
        locationUri = declaration.location.relativeUri,
        locationPos =
          declaration.location.position.map(x => x.startLine + ":" + x.startCol).getOrElse("NA"),
        defGroupId = library.groupId,
        defArtifactId = library.artifactId,
        defVersion = library.version,
        isCompanion = declaration.isImplicitClassCompanionDef,
        numTypeParameters = declaration.typeParameters.size,
        numParameterLists = declaration.parameterLists.size,
        numParameters = declaration.parameterLists.map(_.parameters.size).sum,
        numImplicitParameters =
          declaration.implicitParameterList.map(_.parameters.size).getOrElse(0)
      )
    }
  }

  def export(idx: Index, output: File): Unit = {
    for {
      out <- output.newOutputStream.autoClosed
      writer <- out.asCsvWriter[Output](rfc.withHeader(Output.Header: _*)).autoClosed
      declaration <- idx.implicitDeclarations
      row = Output(declaration)(idx)
    } {
      writer.write(row)
    }
  }

  def run(corpusPath: File): Unit = {
    println("Using build: " + BuildInfo.buildInfoBuildNumber)
    println("Using corpus: " + corpusPath.path.toAbsolutePath)

    val index: Index = Index(corpusPath)
    val output = corpusPath / "implicit-declarations.csv"

    timedTask(
      s"Exporting ${index.implicitDeclarations.size} declarations call sites into $output",
      export(index, output)
    )
  }

  args.toList match {
    case output :: Nil => run(File(output))
    case _ => sys.error("Usage: <corpus path>")
  }
}
