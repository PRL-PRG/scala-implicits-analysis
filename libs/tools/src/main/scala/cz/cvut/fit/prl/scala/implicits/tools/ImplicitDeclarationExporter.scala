package cz.cvut.fit.prl.scala.implicits.tools

import better.files.File
import cz.cvut.fit.prl.scala.implicits.model._
import kantan.csv._
import kantan.csv.ops._
import kantan.csv.generic._

import scala.util.Try

case class ImplicitDeclaration(
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
    compilationUnit: String,
    defGroupId: String,
    defArtifactId: String,
    defVersion: String,
    isImplicit: Boolean,
    isCompanion: Boolean,
    access: String,
    annotations: String,
    numTypeParameters: Int,
    numParameterLists: Int,
    numParameters: Int,
    numImplicitParameters: Int
)

object ImplicitDeclaration {
  object implicits {
    implicit object encoder extends HeaderEncoder[ImplicitDeclaration] {
      override def header: Option[Seq[String]] = Some(
        Seq(
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
          "compilation_unit",
          "def_group_id",
          "def_artifact_id",
          "def_version",
          "is_implicit",
          "is_companion",
          "access",
          "annotations",
          "num_type_parameters",
          "num_parameter_lists",
          "num_parameters",
          "num_implicit_parameters"
        )
      )

      override def rowEncoder: RowEncoder[ImplicitDeclaration] = implicitly
    }
  }
}

object ImplicitDeclarationExporter extends Exporter[ImplicitDeclaration] {

  def encode(declaration: Declaration)(implicit idx: Index): ImplicitDeclaration = {
    val project = declaration.project
    val module = declaration.module
    val library = declaration.library

    // TODO: these kind of things should be in the encoder
    val annotations = {
      val tmp = declaration.annotations.map(_.declarationId).mkString(";")
      if (tmp.isEmpty) "NA" else tmp
    }

    ImplicitDeclaration(
      projectId = project.projectId,
      moduleId = module.moduleId,
      groupId = module.groupId,
      artifactId = module.artifactId,
      version = module.version,
      kind = declaration.kind.name,
      fqn = declaration.declarationId,
      name = declaration.name,
      locationPath = declaration.location.patchedPath,
      locationUri = declaration.location.relativeUri,
      locationPos =
        declaration.location.position.map(x => x.startLine + ":" + x.startCol).getOrElse("NA"),
      locationScope = declaration.locationScope,
      compilationUnit = declaration.compilationUnit.getOrElse("NA"),
      defGroupId = library.groupId,
      defArtifactId = library.artifactId,
      defVersion = library.version,
      isImplicit = declaration.isImplicit,
      isCompanion = declaration.isImplicitClassCompanionDef,
      access = declaration.access.toString(),
      annotations = annotations,
      numTypeParameters = declaration.typeParameters.size,
      numParameterLists = declaration.parameterLists.size,
      numParameters = declaration.parameterLists.map(_.parameters.size).sum,
      numImplicitParameters = declaration.implicitParameterList.map(_.parameters.size).getOrElse(0)
    )
  }

  override def export(project: Project): Stream[Try[ImplicitDeclaration]] = {
    implicit val idx = ProjectIndex(project)

    idx.implicitDeclarations.toStream
      .map { declaration =>
        Try(encode(declaration))
      }
  }
}

import ImplicitDeclaration.implicits.encoder

object ImplicitDeclarationExporterApp
    extends CSVExporterApp[ImplicitDeclaration](
      ImplicitDeclarationExporter,
      File("implicit-declarations.csv"))
