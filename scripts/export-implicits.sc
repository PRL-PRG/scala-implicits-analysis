import $ivy.`cz.cvut.fit.prl.scala.implicits::tools:1.0-SNAPSHOT`
import $ivy.`cz.cvut.fit.prl.scala.implicits::metadata:1.0-SNAPSHOT`

import better.files._
import cz.cvut.fit.prl.scala.implicits.model._
import cz.cvut.fit.prl.scala.implicits.model.Util._
import cz.cvut.fit.prl.scala.implicits.tools._

@main
def main(implicitsPath: String) = {
  val compositeExporter = new CompositeMultiProjectExporter(Seq(
    ImplicitDeclarationExporterApp.csvExporter,
    ImplicitCallSitesExporterApp.csvExporter,
    ImplicitConversionExporterApp.csvExporter
  ))

  for {
    input <- File(implicitsPath).inputStream
    exporter <- compositeExporter.autoClosed
    project <- Project.streamFrom(input)
  } {
    exporter.export(project)
  }
}
