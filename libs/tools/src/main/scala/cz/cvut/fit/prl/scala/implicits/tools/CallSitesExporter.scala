package cz.cvut.fit.prl.scala.implicits.tools

import better.files._

import cz.cvut.fit.prl.scala.implicits.model._

import kantan.csv.generic._

import scala.util.{Failure, Success, Try}

case class ExportedCallSite(
    projectId: String,
    moduleId: String,
    groupId: String,
    artifactId: String,
    version: String,
    code: String,
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

object ExportedCallSite {
  val Header = Seq(
    "project_id",
    "module_id",
    "group_id",
    "artifact_id",
    "version",
    "code",
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
}

object CallSitesExporter extends ExporterApp[CallSite, ExportedCallSite] {
  val OutputFilename = "implicit-callsites.csv"

  private def indexer(idx: Index): Iterator[CallSite] = idx.implicitCallSites.iterator

  private def mapper(cs: CallSite, idx: Index): Either[Problem, ExportedCallSite] =
    Try(map(cs)(idx)) match {
      case Success(output) => Right(output)
      case Failure(e) =>
        Left(
          Problem(
            cs.projectId(idx),
            cs.moduleId,
            s"Unable to convert $cs",
            e.getClass.getSimpleName,
            e.getMessage,
            e.getStackTrace.head.toString
          )
        )
    }

  private def map(callSite: CallSite)(implicit idx: Index): ExportedCallSite = {
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

    ExportedCallSite(
      projectId = project.projectId,
      moduleId = module.moduleId,
      groupId = module.groupId,
      artifactId = module.artifactId,
      version = module.version,
      code = callSite.code,
      declarationId = declaration.declarationId,
      declarationKind = declaration.kind.name,
      declarationGroupId = declarationLibrary.groupId,
      declarationArtifactId = declarationLibrary.artifactId,
      declarationVersion = declarationLibrary.version,
      local = local,
      locationPath = callSite.location.path,
      locationUri = callSite.location.relativeUri,
      locationPos =
        callSite.location.position.map(x => x.startLine + ":" + x.startCol).getOrElse("NA"),
      locationScope = callSite.locationScope.getOrElse("NA"),
      numTypeArguments = callSite.typeArguments.size,
      numImplicitArguments = callSite.implicitArgumentTypes.size
    )
  }

  def exporter(outputFile: File): ProjectExporter[CallSite, ExportedCallSite] = {
    val dir = outputFile.parent
    val problemsFile = dir / (outputFile.nameWithoutExtension + "-problems.csv")

    val writer = new CSVWriter[ExportedCallSite](outputFile, ExportedCallSite.Header)
    val problemsWriter = new CSVWriter[Problem](problemsFile, Problem.Header)

    new ProjectExporter[CallSite, ExportedCallSite](
      writer,
      problemsWriter,
      indexer,
      mapper
    )
  }

  override def createExporter(): ProjectExporter[CallSite, ExportedCallSite] =
    exporter(File(OutputFilename))
}
