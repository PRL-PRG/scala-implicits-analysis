package cz.cvut.fit.prl.scala.implicits.extractor

import scala.meta.internal.{semanticdb => s}

sealed trait ConversionException {
  def uri: String
  def location: Option[s.Range]
  def code: String
  def getCause: Throwable

  def longSummary: String = {
    val position = uri + location.map(x => s":${x.startLine}:${x.startCharacter}").getOrElse("")
    s"$summary :: $position :: $code"
  }

  def summary: String = {
    val kind = getCause.getClass.getSimpleName

    s"FAILURE :: $kind :: ${getCause.getMessage}"
  }

}

case class DeclarationConversionException(
    cause: Throwable,
    uri: String,
    location: Option[s.Range],
    symbolInfo: s.SymbolInformation)
    extends Exception(s"Unable to convert ${symbolInfo.symbol} ($uri}: ${cause.getMessage}", cause)
    with ConversionException {
  override def code: String = symbolInfo.symbol
}

case class CallSiteConversionException(cause: Throwable, uri: String, synthetic: s.Synthetic)
    extends Exception(s"Unable to convert ${synthetic} ($uri}: ${cause.getMessage}", cause)
    with ConversionException {
  override def code: String = synthetic.toString
  override def location: Option[s.Range] = None
}

case class SkippedSymbolException(what: String) extends Exception(s"Skipped symbol $what")

case class UnsupportedElementException[T](what: String, actual: T) extends Exception(s"Unsupported $what ($actual)")

case class UnexpectedElementException[T](what: String, actual: T) extends Exception(s"Unexpected $what ($actual)")

case class MissingSymbolException(message: String) extends Exception
