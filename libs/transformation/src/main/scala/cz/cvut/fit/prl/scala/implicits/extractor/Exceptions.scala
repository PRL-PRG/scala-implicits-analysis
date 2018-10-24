package cz.cvut.fit.prl.scala.implicits.extractor

import scala.meta.internal.{semanticdb => s}

case class ConversionException(
    cause: Throwable,
    uri: String,
    location: Option[s.Range],
    symbolInfo: s.SymbolInformation)
    extends Exception(s"Unable to convert ${symbolInfo.symbol} ($uri}: ${cause.getMessage}", cause) {

  def longSummary: String = {
    val position = uri + location.map(x => s":${x.startLine}:${x.startCharacter}").getOrElse("")
    s"$summary :: $position :: ${symbolInfo.symbol}"
  }

  def summary: String = {
    val kind = cause.getClass.getSimpleName

    s"FAILURE :: $kind :: ${cause.getMessage}"
  }

}

case class SkippedSymbolException(what: String) extends Exception(s"Skipped symbol $what")

case class UnsupportedElementException[T](what: String, actual: T) extends Exception(s"Unsupported $what ($actual)")

case class UnexpectedElementException[T](what: String, actual: T) extends Exception(s"Unexpected $what ($actual)")

case class MissingSymbolException(message: String) extends Exception
