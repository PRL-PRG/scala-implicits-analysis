package cz.cvut.fit.prl.scala.implicits.tools

case class ExportError(
    projectId: String,
    moduleId: String,
    message: String,
    exception: String,
    exceptionMessage: String,
    exceptionTrace: String
)

object ExportError {
  val Header: Seq[String] = Seq(
    "project_id",
    "module_id",
    "message",
    "exception",
    "exception_message",
    "exception_trace"
  )
}
