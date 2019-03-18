package cz.cvut.fit.prl.scala.implicits.tools

import better.files._

import cz.cvut.fit.prl.scala.implicits.model._

import kantan.csv.generic._

import scala.util.{Failure, Success, Try}

case class ExportedDeclaration(
    projectId: String,
    moduleId: String,
    groupId: String,
    artifactId: String,
    version: String,
    kind: String,
    fqn: String,
    name: String,
    locationPath: String,
    locationUri: String,
    locationPos: String,
    locationScope: String,
    defGroupId: String,
    defArtifactId: String,
    defVersion: String,
    isCompanion: Boolean,
    numTypeParameters: Int,
    numParameterLists: Int,
    numParameters: Int,
    numImplicitParameters: Int
)

object ExportedDeclaration {
  val Header = Seq(
    "project_id",
    "module_id",
    "group_id",
    "artifact_id",
    "version",
    "kind",
    "fqn",
    "name",
    "location_path",
    "location_uri",
    "location_pos",
    "location_scope",
    "def_group_id",
    "def_artifact_id",
    "def_version",
    "is_companion",
    "num_type_parameters",
    "num_parameter_lists",
    "num_parameters",
    "num_implicit_parameters"
  )
}

object DeclarationsExporter extends ExporterApp[Declaration, ExportedDeclaration] {
  val OutputFilename = "implicit-declarations.csv"

  private def indexer(idx: Index): Iterator[Declaration] = idx.implicitDeclarations.iterator

  private def mapper(declaration: Declaration, idx: Index): Either[Problem, ExportedDeclaration] =
    Try(map(declaration)(idx)) match {
      case Success(output) => Right(output)
      case Failure(e) =>
        Left(
          Problem(
            declaration.projectId(idx),
            declaration.moduleId,
            s"Unable to convert $declaration",
            e.getClass.getSimpleName,
            e.getMessage,
            e.getStackTrace.head.toString
          )
        )
    }

  def map(declaration: Declaration)(implicit idx: Index): ExportedDeclaration = {
    val project = declaration.project
    val module = declaration.module
    val library = declaration.library

    ExportedDeclaration(
      projectId = project.projectId,
      moduleId = module.moduleId,
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
      locationScope = declaration.locationScope.getOrElse("NA"),
      defGroupId = library.groupId,
      defArtifactId = library.artifactId,
      defVersion = library.version,
      isCompanion = declaration.isImplicitClassCompanionDef,
      numTypeParameters = declaration.typeParameters.size,
      numParameterLists = declaration.parameterLists.size,
      numParameters = declaration.parameterLists.map(_.parameters.size).sum,
      numImplicitParameters = declaration.implicitParameterList.map(_.parameters.size).getOrElse(0)
    )
  }

  def exporter(outputFile: File): ProjectExporter[Declaration, ExportedDeclaration] = {
    val dir = outputFile.parent
    val problemsFile = dir / (outputFile.nameWithoutExtension + "-problems.csv")

    val writer = new CSVWriter[ExportedDeclaration](outputFile, ExportedDeclaration.Header)
    val problemsWriter = new CSVWriter[Problem](problemsFile, Problem.Header)

    new ProjectExporter[Declaration, ExportedDeclaration](
      writer,
      problemsWriter,
      indexer,
      mapper
    )
  }

  override def createExporter(): ProjectExporter[Declaration, ExportedDeclaration] =
    exporter(File(OutputFilename))
}
