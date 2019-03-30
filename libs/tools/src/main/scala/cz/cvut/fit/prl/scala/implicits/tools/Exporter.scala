package cz.cvut.fit.prl.scala.implicits.tools
import cz.cvut.fit.prl.scala.implicits.model.Project

import scala.util.Try

trait Exporter[T] {
  def export(project: Project): Stream[Try[T]]
}
