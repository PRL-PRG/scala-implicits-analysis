package cz.cvut.fit.prl.scala.implicits.tools
import cz.cvut.fit.prl.scala.implicits.model.Project

import scala.util.Try

class CompositeMultiProjectExporter(exporters: Seq[MultiProjectExporter]) extends MultiProjectExporter {
  override def export(project: Project): Unit = {
    exporters.foreach(x => Try(x.export(project)))
  }

  override def close(): Unit = {
    exporters.map(x => Try(x.close())).foreach(_.get)
  }
}
