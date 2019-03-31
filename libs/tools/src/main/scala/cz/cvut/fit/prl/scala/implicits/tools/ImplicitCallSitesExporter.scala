package cz.cvut.fit.prl.scala.implicits.tools

import better.files.File
import cz.cvut.fit.prl.scala.implicits.model._

import scala.util.Try

object ImplicitCallSitesExporter extends Exporter[ImplicitCallSite] {

  def encode(callSite: CallSite)(implicit idx: Index): ImplicitCallSite = {
    ImplicitCallSite(callSite)
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
