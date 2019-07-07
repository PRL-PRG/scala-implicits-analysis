package cz.cvut.fit.prl.scala.implicits.tools

import cz.cvut.fit.prl.scala.implicits.model._
import kantan.csv.{HeaderEncoder, RowEncoder}

import scala.util.Try


case class ImplicitParameter(parameter: Parameter, declaringType: Declaration)(implicit idx: Index) {
  private implicit val module: Module = declaringType.module
  private val library = declaringType.library

  private val returnType = parameter.tpe
  private val returnDeclaration = returnType.declaration
  private val resolvedReturnDeclaration = Try(returnDeclaration.resolveType(module)).toOption
  private val returnLibrary = returnDeclaration.library

  def projectId: String = module.projectId
  def moduleId: String = module.moduleId
  def groupId: String = module.groupId
  def artifactId: String = module.artifactId
  def version: String = module.version
  def declarationId: String = declaringType.declarationId
  def declarationKind: Declaration.Kind = declaringType.kind
  def declarationIsImplicit: Boolean = declaringType.isImplicit
  def declarationIsCompanion: Boolean = declaringType.isImplicitClassCompanionDef(module)
  def defGroupId: String = library.groupId
  def defArtifactId: String = library.artifactId
  def defVersion: String = library.version
  def defLocationScope: String = declaringType.locationScope
  def defLocationPath: String = declaringType.location.patchedPath
  def defLocationUri: String = declaringType.location.relativeUri
  def defLocationGithub: Option[String] = declaringType.githubURL
  def parameterId: String = s"${declaringType.declarationId}(${parameter.name})"
  def name: String = parameter.name
  def code: String = returnType.asCode
  def typeId: String = returnDeclaration.declarationId
  def typeKind: Declaration.Kind = returnDeclaration.kind
  def typeGroupId: String = returnLibrary.groupId
  def typeArtifactId: String = returnLibrary.artifactId
  def typeVersion: String = returnLibrary.version
  def typeLocationScope: String = returnDeclaration.locationScope
  def typeLocationPath: String = returnDeclaration.location.patchedPath
  def typeLocationUri: String = returnDeclaration.location.relativeUri
  def typeLocationGithub: Option[String] = returnDeclaration.githubURL
  def typeLocal: String =
    if (library == returnLibrary) {
      "module"
    } else if (library.groupId == returnLibrary.groupId) {
      "project"
    } else {
      "NA"
    }
  def resolvedTypeId: Option[String] = resolvedReturnDeclaration.map(_.declarationId)
  def resolvedTypeKind: Option[Declaration.Kind] = resolvedReturnDeclaration.map(_.kind)
  def numTypeArguments: Int = returnType.typeArguments.size
  def numTypeArgumentRefs: Int = parameter.tpe.typeArguments.count { x =>
    x.declaration.kind.isTypeParameter
  }
}

object ImplicitParameter {
  object implicits {
    implicit object encoder extends HeaderEncoder[ImplicitParameter] {
      override def header: Option[Seq[String]] = Some(
        Seq(
          "project_id",
          "module_id",
          "group_id",
          "artifact_id",
          "version",
          "declaration_id",
          "declaration_kind",
          "declaration_is_implicit",
          "declaration_is_companion",
          "def_group_id",
          "def_artifact_id",
          "def_version",
          "def_location_scope",
          "def_location_path",
          "def_location_uri",
          "def_location_github",
          "parameter_id",
          "name",
          "code",
          "type_id",
          "type_kind",
          "type_group_id",
          "type_artifact_id",
          "type_version",
          "type_location_scope",
          "type_location_path",
          "type_location_uri",
          "type_location_github",
          "type_local",
          "resolved_type_id",
          "resolved_type_kind",
          "num_type_arguments",
          "num_type_argument_refs"
        )
      )

      override def rowEncoder: RowEncoder[ImplicitParameter] =
        (d: ImplicitParameter) => {
          import CSVExporter.encoders._
          Seq[Value](
            d.projectId,
            d.moduleId,
            d.groupId,
            d.artifactId,
            d.version,
            d.declarationId,
            d.declarationKind,
            d.declarationIsImplicit,
            d.declarationIsCompanion,
            d.defGroupId,
            d.defArtifactId,
            d.defVersion,
            d.defLocationScope,
            d.defLocationPath,
            d.defLocationUri,
            d.defLocationGithub,
            d.parameterId,
            d.name,
            d.code,
            d.typeId,
            d.typeKind,
            d.typeGroupId,
            d.typeArtifactId,
            d.typeVersion,
            d.typeLocationScope,
            d.typeLocationPath,
            d.typeLocationUri,
            d.typeLocationGithub,
            d.typeLocal,
            d.resolvedTypeId,
            d.resolvedTypeKind,
            d.numTypeArguments,
            d.numTypeArgumentRefs
          ).map(_.str)
        }
    }
  }
}
