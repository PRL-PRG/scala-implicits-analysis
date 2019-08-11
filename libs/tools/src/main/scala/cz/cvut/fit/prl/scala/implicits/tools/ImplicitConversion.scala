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
  def moduleId: String = module.moduleId
  def groupId: String = module.groupId
  def artifactId: String = module.artifactId
  def version: String = module.version
  def declarationId: String = declaration.declarationId
  def isLocal: Boolean = declaration.isProjectLocal || declaration.isImplicitClassCompanionDef(module)
  def fromType: String = from.resolveFullType.asCode
  def fromGroupId: String = fromLibrary.groupId
  def fromArtifactId: String = fromLibrary.artifactId
  def fromVersion: String = fromLibrary.version
  def fromScope: String = fromDeclaration.locationScope
  def fromCompilationUnit: Option[String] = fromDeclaration.compilationUnit
  def fromLanguage: Language = fromDeclaration.language
  def fromLocationPath: String = fromDeclaration.location.patchedPath
  def fromLocationUri: String = fromDeclaration.location.relativeUri
  def toType: String = to.resolveFullType.asCode
  def toGroupId: String = toLibrary.groupId
  def toArtifactId: String = toLibrary.artifactId
  def toVersion: String = toLibrary.version
  def toScope: String = toDeclaration.locationScope
  def toCompilationUnit: Option[String] = toDeclaration.compilationUnit
  def toLanguage: Language = toDeclaration.language
  def toLocationPath: String = toDeclaration.location.patchedPath
  def toLocationUri: String = toDeclaration.location.relativeUri
  def toIsTrait: Boolean = toDeclaration.isTraitOrInterface
  def toExtendsTrait: Boolean =
    toDeclaration.isTraitOrInterface |
    toDeclaration.classSignature.parents.exists(_.declaration.isTraitOrInterface)
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
          "declaration_id",
          "is_local",
          "from",
          "from_group_id",
          "from_artifact_id",
          "from_version",
          "from_scope",
          "from_compilation_unit",
          "from_language",
          "from_location_path",
          "from_location_uri",
          "to",
          "to_group_id",
          "to_artifact_id",
          "to_version",
          "to_scope",
          "to_compilation_unit",
          "to_language",
          "to_location_path",
          "to_location_uri",
          "to_is_trait",
          "to_extends_trait"
        )
      )

      override def rowEncoder: RowEncoder[ImplicitConversion] =
        (d: ImplicitConversion) => {
          import CSVExporter.encoders._
          Seq[Value](
            d.projectId,
            d.moduleId,
            d.groupId,
            d.artifactId,
            d.version,
            d.declarationId,
            d.isLocal,
            d.fromType,
            d.fromGroupId,
            d.fromArtifactId,
            d.fromVersion,
            d.fromScope,
            d.fromCompilationUnit,
            d.fromLanguage,
            d.fromLocationPath,
            d.fromLocationUri,
            d.toType,
            d.toGroupId,
            d.toArtifactId,
            d.toVersion,
            d.toScope,
            d.toCompilationUnit,
            d.toLanguage,
            d.toLocationPath,
            d.toLocationUri,
            d.toIsTrait,
            d.toExtendsTrait
          ).map(_.str)
        }
    }
  }
}
