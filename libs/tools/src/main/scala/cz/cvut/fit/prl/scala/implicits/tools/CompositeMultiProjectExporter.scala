package cz.cvut.fit.prl.scala.implicits.tools
import com.typesafe.scalalogging.Logger
import cz.cvut.fit.prl.scala.implicits.model.Util.timedTask
import cz.cvut.fit.prl.scala.implicits.model.Project
import org.slf4j.LoggerFactory

import scala.util.Try

class CompositeMultiProjectExporter(exporters: Seq[MultiProjectExporter]) extends MultiProjectExporter {
  implicit val logger: Logger = Logger(LoggerFactory.getLogger(getClass.getName))
  private var exported = 0

  override def export(project: Project): Unit = {
    exported = exported + 1
    timedTask(s"Exporting project ($exported) ${project.projectId}") {
      exporters.foreach(x => Try(x.export(project)))
    }
  }

  override def close(): Unit = {
    exported = 0
    exporters.map(x => Try(x.close())).foreach(_.get)
  }
}
