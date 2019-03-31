package cz.cvut.fit.prl.scala.implicits.tools

import better.files.File
import cz.cvut.fit.prl.scala.implicits.model._
import cats.Monoid
import cats.implicits._
import kantan.csv._
import kantan.csv.ops._
import kantan.csv.generic._

import scala.util.Try

case class ImplicitCallSite(
    projectId: String,
    moduleId: String,
    groupId: String,
    artifactId: String,
    version: String,
    code: String,
    callSiteId: Int,
    parentId: String,
    nestedCalls: String,
    arguments: String,
    declarationId: String,
    declarationKind: String,
    declarationGroupId: String,
    declarationArtifactId: String,
    declarationVersion: String,
    local: String,
    locationPath: String,
    locationUri: String,
    locationPos: String,
    locationScope: String,
    numTypeArguments: Int,
    numImplicitArguments: Int
)

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
          "code",
          "callsite_id",
          "parent_id",
          "nested_calls",
          "arguments",
          "declaration_id",
          "declaration_kind",
          "declaration_group_id",
          "declaration_artifact_id",
          "declaration_version",
          "local",
          "location_path",
          "location_uri",
          "location_pos",
          "location_scope",
          "num_type_arguments",
          "num_implicit_arguments"
        )
      )

      override def rowEncoder: RowEncoder[ImplicitCallSite] = implicitly
    }
  }
}

object ImplicitCallSitesExporter extends Exporter[ImplicitCallSite] {

  def encode(callSite: CallSite)(implicit idx: Index): ImplicitCallSite = {
    val project = callSite.project
    val module = callSite.module
    val library = callSite.library
    val declaration = callSite.declaration
    val declarationLibrary = declaration.library
    val local =
      if (library == declarationLibrary) {
        "module"
      } else if (library.groupId == declarationLibrary.groupId) {
        "project"
      } else {
        "NA"
      }
    val callSiteId = callSite.callSiteId
    val parentId = callSite.parentId.map(_.toString).getOrElse("NA")
    val nestedCalls = callSite.implicitArgumentTypes.collect {
      case CallSiteRef(id) => id
    }.mkString(";")
    val arguments = callSite.implicitArgumentTypes.collect {
      case ValueRef(declarationId) => module.resolveDeclaration(declarationId).resolveType.declarationId
    }.mkString(";")

    // TODO -- the NA should be handled somewhere else
    ImplicitCallSite(
      projectId = project.projectId,
      moduleId = module.moduleId,
      groupId = module.groupId,
      artifactId = module.artifactId,
      version = module.version,
      code = callSite.code,
      callSiteId = callSiteId,
      parentId = parentId,
      nestedCalls = if (!nestedCalls.isEmpty) nestedCalls else "NA",
      arguments = if (!arguments.isEmpty) arguments else "NA",
      declarationId = declaration.declarationId,
      declarationKind = declaration.kind.name,
      declarationGroupId = declarationLibrary.groupId,
      declarationArtifactId = declarationLibrary.artifactId,
      declarationVersion = declarationLibrary.version,
      local = local,
      locationPath = callSite.location.patchedPath,
      locationUri = callSite.location.relativeUri,
      locationPos =
        callSite.location.position.map(x => x.startLine + ":" + x.startCol).getOrElse("NA"),
      locationScope = callSite.locationScope,
      numTypeArguments = callSite.typeArguments.size,
      numImplicitArguments = callSite.implicitArgumentTypes.size
    )
  }

  override def export(project: Project): Stream[Try[ImplicitCallSite]] = {
    implicit val idx: Index = ProjectIndex(project)

    idx.implicitCallSites.toStream.map { callSite =>
      Try(encode(callSite))
    }
  }
}

import ImplicitCallSite.implicits.encoder

object ImplicitCallSitesExporterApp
    extends CSVExporterApp[ImplicitCallSite](
      ImplicitCallSitesExporter,
      File("implicit-callsites.csv")
    )
