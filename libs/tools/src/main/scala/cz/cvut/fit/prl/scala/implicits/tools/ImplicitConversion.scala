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
  def declarationId: String = declaration.declarationId
  def isLocal: Boolean = declaration.isProjectLocal || declaration.isImplicitClassCompanionDef(module)
  def fromType: String = from.resolveFullType.asCode
  def fromGroupId: String = fromLibrary.groupId
  def fromArtifactId: String = fromLibrary.artifactId
  def fromVersion: String = fromLibrary.version
  def fromScope: String = fromDeclaration.locationScope
  def fromCompilationUnit: Option[String] = fromDeclaration.compilationUnit
  def fromLanguage: Language = fromDeclaration.language
  def toType: String = to.resolveFullType.asCode
  def toGroupId: String = toLibrary.groupId
  def toArtifactId: String = toLibrary.artifactId
  def toVersion: String = toLibrary.version
  def toScope: String = toDeclaration.locationScope
  def toCompilationUnit: Option[String] = toDeclaration.compilationUnit
  def toLanguage: Language = toDeclaration.language
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
          "declaration_id",
          "is_local",
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
            d.declarationId,
            d.isLocal,
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
            d.toIsTrait,
            d.toExtendsTrait
          ).map(_.str)
        }
    }
  }
}
