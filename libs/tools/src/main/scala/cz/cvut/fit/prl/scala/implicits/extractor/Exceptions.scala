package cz.cvut.fit.prl.scala.implicits.extractor

import scala.meta.internal.{semanticdb => s}

class DeclarationConversionException(
    cause: Throwable,
    uri: String,
    location: Option[s.Range],
    symbolInfo: s.SymbolInformation)
    extends Exception(s"Unable to convert ${symbolInfo.symbol} ($uri}: ${cause.getMessage}", cause)

class CallSiteConversionException(cause: Throwable, uri: String, synthetic: s.Synthetic)
    extends Exception(s"Unable to convert call site $synthetic ($uri}: ${cause.getMessage}", cause)

class SkippedSymbolException(symbol: String) extends Exception(s"Skipped symbol $symbol")

class ElementNotSupportedException[T](what: String, actual: T)
    extends Exception(s"Unsupported $what ($actual)")

case class ElementNotExpectedException[T](what: String, actual: T)
    extends Exception(s"Unexpected $what ($actual)")

case class SymbolNotFoundException(message: String) extends Exception(message)

case class FunctionNotFoundException(term: String, range: s.Range, uri: String)
  extends Exception(s"Missing function for $term $uri:$range")

case class LoadingMetadataException(message: String) extends Exception(message)

case class ImplicitArgumentNotFoundException(code: String, declarationId: String)
    extends Exception(s"$code -- $declarationId")

case class UnExtractableCallSiteException(synthetic: s.Synthetic, uri: String, cause: Throwable = null)
    extends Exception(
      s"Unable to extract ${synthetic.tree} into a call site $uri:${synthetic.range.getOrElse("???")}",
      cause
    )
