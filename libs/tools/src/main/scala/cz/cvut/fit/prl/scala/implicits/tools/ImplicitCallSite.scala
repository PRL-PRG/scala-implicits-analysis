package cz.cvut.fit.prl.scala.implicits.tools
import cz.cvut.fit.prl.scala.implicits.model.Declaration.Kind
import cz.cvut.fit.prl.scala.implicits.model._
import kantan.csv.{HeaderEncoder, RowEncoder}

case class ImplicitCallSite(callSite: CallSite)(implicit idx: Index) {
  private val module = callSite.module
  private val library = callSite.library
  private val declaration = callSite.declaration
  private val declarationLibrary = declaration.library

  def projectId: String = module.projectId
  def moduleId: String = module.moduleId
  def groupId: String = module.groupId
  def artifactId: String = module.artifactId
  def version: String = module.version
  def code: String = {
    val tmp = if (callSite.code.length > 50) {
      callSite.code.substring(0, 50) + "..."
    } else {
      callSite.code
    }
    tmp.replaceAll("\n", " ")
  }
  def callSiteId: Int = callSite.callSiteId
  def parentId: Option[Int] = callSite.parentId
  def nestedCalls: Seq[Int] = callSite.implicitArgumentTypes.collect {
    case CallSiteRef(id) => id
  }
  def arguments: Seq[String] = callSite.implicitArgumentTypes.collect {
    case ValueRef(declarationId) =>
      module.resolveDeclaration(declarationId).resolveType.declarationId
  }
  def declarationId: String = declaration.declarationId
  def local: String =
    if (library == declarationLibrary) {
      "module"
    } else if (library.groupId == declarationLibrary.groupId) {
      "project"
    } else {
      "NA"
    }
  def locationPath: String = callSite.location.patchedPath
  def locationUri: String = callSite.location.relativeUri
  def locationPos: Option[Position] = callSite.location.position
  def locationScope: String = callSite.locationScope
  def locationGithub: Option[String] = callSite.githubURL
  def numTypeArguments: Int = callSite.typeArguments.size
  def numImplicitArguments: Int = callSite.implicitArgumentTypes.size
  def githubUrl: Option[String] = callSite.githubURL
}

object ImplicitCallSite {
  object implicits {
    implicit object encoder extends HeaderEncoder[ImplicitCallSite] {
      override def header: Option[Seq[String]] = Some(
        Seq(
          "project_id",
          "module_id",
          "group_id",
          "artifact_id",
          "version",
          "callsite_id",
          "parent_id",
          "code",
          "nested_calls",
          "arguments",
          "declaration_id",
          "local",
          "location_path",
          "location_uri",
          "location_pos",
          "location_scope",
          "num_type_arguments",
          "num_implicit_arguments",
          "github_url"
        )
      )

      override def rowEncoder: RowEncoder[ImplicitCallSite] =
        (cs: ImplicitCallSite) => {
          import CSVExporter.encoders._
          Seq[Value](
            cs.projectId,
            cs.moduleId,
            cs.groupId,
            cs.artifactId,
            cs.version,
            cs.callSiteId,
            cs.parentId,
            cs.code,
            cs.nestedCalls,
            cs.arguments,
            cs.declarationId,
            cs.local,
            cs.locationPath,
            cs.locationUri,
            cs.locationPos,
            cs.locationScope,
            cs.numTypeArguments,
            cs.numImplicitArguments,
            cs.githubUrl
          ).map(_.str)
        }
    }
  }
}
