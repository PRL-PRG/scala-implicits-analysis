package cz.cvut.fit.prl.scala.implicits.tools
import cz.cvut.fit.prl.scala.implicits.model.Project

trait MultiProjectExporter extends AutoCloseable {
  def export(project: Project): Unit
}