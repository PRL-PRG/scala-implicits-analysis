package cz.cvut.fit.prl.scala.implicits.tools

import better.files._

import com.typesafe.scalalogging.Logger

import cz.cvut.fit.prl.scala.implicits.model._
import cz.cvut.fit.prl.scala.implicits.model.Util.timedTask

import cats.Monoid
import cats.implicits._

import kantan.csv._
import kantan.csv.ops._
import kantan.csv.generic._

import org.slf4j.LoggerFactory

import scala.util.{Failure, Success, Try}

object ExportImplicitCallSites extends ExportApp {

  implicit val logger = Logger(LoggerFactory.getLogger(getClass.getName))

  case class Output(
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

  object Output {
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

    def apply(callSite: CallSite)(implicit idx: Index): Output = {
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

      Output(
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
  }

  private def export(idx: Index, outputFile: File): (Int, Int) = {
    val pipe = for {
      out <- outputFile.outputStream
      writer <- out.asCsvWriter[Output](rfc.withHeader(Output.Header: _*)).autoClosed
      callSite <- idx.implicitCallSites
    } yield
      Try(Output(callSite)(idx)) match {
        case Success(row) =>
          writer.write(row)
          (1, 0)
        case Failure(e) =>
          reportException(
            callSite.projectId(idx),
            callSite.moduleId,
            s"Unable to convert $callSite",
            e)
          (0, 1)
      }

    pipe.foldLeft(Monoid[(Int, Int)].empty)(_ |+| _)
  }

  def run(idx: Index, outputFile: File): Unit = {
    timedTask(s"Exporting ${idx.implicitCallSites.size} call sites into $outputFile") {
      val (s, f) = export(idx, outputFile)
      logger.info(s"Exported $s call sites, $f failed")
    }
  }

  def defaultOutputFilename = "implicit-callsites.csv"
}
