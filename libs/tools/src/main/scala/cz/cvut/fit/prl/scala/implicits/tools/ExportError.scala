package cz.cvut.fit.prl.scala.implicits.tools

import kantan.csv._
import kantan.csv.generic._

case class ExportError(
    projectId: String,
    moduleId: String,
    message: String,
    exception: String,
    exceptionMessage: String,
    exceptionTrace: String
)

object ExportError {
  object implicits {
    implicit object encoder extends HeaderEncoder[ExportError] {
      override def header: Option[Seq[String]] = Some(
        Seq(
          "project_id",
          "module_id",
          "message",
          "exception",
          "exception_message",
          "exception_trace"
        )
      )

      override def rowEncoder: RowEncoder[ExportError] = implicitly
    }
  }
}
