package cz.cvut.fit.prl.scala.implicits.tools

import cz.cvut.fit.prl.scala.implicits.model._
import kantan.csv.{HeaderEncoder, RowEncoder}

case class ImplicitConversion(
    declaration: Declaration,
    from: TypeRef,
    to: TypeRef
)(implicit idx: Index) {
  private implicit val module: Module = declaration.module

  private val fromDeclaration: Declaration = from.declaration
  private val fromLibrary: Library = fromDeclaration.library

  private val toDeclaration: Declaration = to.declaration
  private val toLibrary: Library = toDeclaration.library

  def projectId: String = module.projectId
  def moduleId: String = declaration.moduleId
  def groupId: String = module.groupId
  def artifactId: String = module.artifactId
  def version: String = module.version
  def kind: String = declaration.kind.name
  def fqn: String = declaration.declarationId
  def name: String = declaration.name
  def locationPath: String = declaration.location.patchedPath
  def locationUri: String = declaration.location.relativeUri
  def locationPos: String =
    declaration.location.position
      .map(x => x.startLine + ":" + x.startCol)
      .getOrElse("NA")
  def locationScope: String = declaration.locationScope
  def compilationUnit: String = declaration.compilationUnit.getOrElse("NA")
  def fromType: String = from.resolveFullType.asCode
  def fromGroupId: String = fromLibrary.groupId
  def fromArtifactId: String = fromLibrary.artifactId
  def fromVersion: String = fromLibrary.version
  def fromScope: String = fromDeclaration.locationScope
  def fromCompilationUnit: String = fromDeclaration.compilationUnit.getOrElse("NA")
  def fromLanguage: String = fromDeclaration.language.toString()
  def toType: String = to.resolveFullType.asCode
  def toGroupId: String = toLibrary.groupId
  def toArtifactId: String = toLibrary.artifactId
  def toVersion: String = toLibrary.version
  def toScope: String = toDeclaration.locationScope
  def toCompilationUnit: String = toDeclaration.compilationUnit.getOrElse("NA")
  def toLanguage: String = toDeclaration.language.toString()
  def numImplicitParameters: String =
    declaration.implicitParameterList
      .map(_.parameters.size)
      .getOrElse(0)
      .toString
  def numTypeParameters: String = declaration.typeParameters.size.toString

}

object ImplicitConversion {
  object implicits {
    implicit object encoder extends HeaderEncoder[ImplicitConversion] {
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
          "from",
          "from_groupId",
          "from_artifactId",
          "from_version",
          "from_scope",
          "from_compilation_unit",
          "from_language",
          "to",
          "to_groupId",
          "to_artifactId",
          "to_version",
          "to_scope",
          "to_compilation_unit",
          "to_language",
          "num_implicit_parameters",
          "num_type_parameters"
        )
      )

      override def rowEncoder: RowEncoder[ImplicitConversion] =
        (d: ImplicitConversion) =>
          Seq(
            d.projectId,
            d.moduleId,
            d.groupId,
            d.artifactId,
            d.version,
            d.kind,
            d.fqn,
            d.name,
            d.locationPath,
            d.locationUri,
            d.locationPos,
            d.locationScope,
            d.compilationUnit,
            d.fromType,
            d.fromGroupId,
            d.fromArtifactId,
            d.fromVersion,
            d.fromScope,
            d.fromCompilationUnit,
            d.fromLanguage,
            d.toType,
            d.toGroupId,
            d.toArtifactId,
            d.toVersion,
            d.toScope,
            d.toCompilationUnit,
            d.toLanguage,
            d.numImplicitParameters,
            d.numTypeParameters
        )
    }
  }
}
