package cz.cvut.fit.prl.scala.implicits.tools

import better.files.File
import cz.cvut.fit.prl.scala.implicits.model._

import scala.util.Try

object ImplicitCallSitesExporter extends Exporter[ImplicitCallSite] {

  def encode(callSite: CallSite, csIdx: Int => CallSite)(implicit idx: Index): ImplicitCallSite = {
    ImplicitCallSite(callSite, csIdx)
  }

  override def export(project: Project): Stream[Try[ImplicitCallSite]] = {
    implicit val idx: Index = ProjectIndex(project)

    idx.modules.toStream.flatMap { m =>
      val css = m.implicitCallSites
      val csIdx = css.map(x => x.callSiteId -> x).toMap

      css.toStream.map(callSite =>Try(encode(callSite, csIdx)))
    }
  }
}

import ImplicitCallSite.implicits.encoder

object ImplicitCallSitesExporterApp
    extends CSVExporterApp[ImplicitCallSite](
      ImplicitCallSitesExporter,
      File("implicit-callsites.csv")
    )
