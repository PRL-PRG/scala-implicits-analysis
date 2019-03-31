package cz.cvut.fit.prl.scala.implicits.tools
import cz.cvut.fit.prl.scala.implicits.model.Declaration.Access
import cz.cvut.fit.prl.scala.implicits.model.{Declaration, Index, Position}
import kantan.csv.{HeaderEncoder, RowEncoder}

import scala.language.implicitConversions

case class ImplicitDeclaration(declaration: Declaration)(implicit idx: Index) {
  private val module = declaration.module
  private val library = declaration.library

  def projectId: String = module.projectId
  def moduleId: String = module.moduleId
  def groupId: String = module.groupId
  def artifactId: String = module.artifactId
  def version: String = module.version
  def kind: Declaration.Kind = declaration.kind
  def declarationId: String = declaration.declarationId
  def name: String = declaration.name
  def locationPath: String = declaration.location.patchedPath
  def locationUri: String = declaration.location.relativeUri
  def locationPos: Option[Position] = declaration.location.position
  def locationScope: String = declaration.locationScope
  def compilationUnit: Option[String] = declaration.compilationUnit
  def defGroupId: String = library.groupId
  def defArtifactId: String = library.artifactId
  def defVersion: String = library.version
  def isImplicit: Boolean = declaration.isImplicit
  def isCompanion: Boolean = declaration.isImplicitClassCompanionDef
  def access: Access = declaration.access
  def annotations: Seq[String] = declaration.annotations.map(_.declarationId)
  def numTypeParameters: Int = declaration.typeParameters.size
  def numParameterLists: Int = declaration.parameterLists.size
  def numParameters: Int = declaration.parameterLists.map(_.parameters.size).sum
  def numImplicitParameters: Option[Int] = declaration.implicitParameterList.map(_.parameters.size)
  def githubUrl: Option[String] = declaration.githubURL
}

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
          "declaration_id",
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
          "num_implicit_parameters",
          "github_url"
        )
      )


      override def rowEncoder: RowEncoder[ImplicitDeclaration] =
        (d: ImplicitDeclaration) => {
          import CSVExporter.encoders._
          Seq[Value](
            d.projectId,
            d.moduleId,
            d.groupId,
            d.artifactId,
            d.version,
            d.kind.name,
            d.declarationId,
            d.name,
            d.locationPath,
            d.locationUri,
            d.locationPos,
            d.locationScope,
            d.compilationUnit,
            d.defGroupId,
            d.defArtifactId,
            d.defVersion,
            d.isImplicit,
            d.isCompanion,
            d.access.name,
            d.annotations,
            d.numTypeParameters,
            d.numParameterLists,
            d.numParameters,
            d.numImplicitParameters,
            d.githubUrl
          ).map(_.str)
        }
    }
  }
}
