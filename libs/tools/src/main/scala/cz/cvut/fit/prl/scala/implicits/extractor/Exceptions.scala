package cz.cvut.fit.prl.scala.implicits.extractor

import scala.meta.internal.{semanticdb => s}

class DeclarationConversionException(
    cause: Throwable,
    uri: String,
    location: Option[s.Range],
    symbolInfo: s.SymbolInformation)
    extends Exception(s"Unable to convert ${symbolInfo.symbol} ($uri}: ${cause.getMessage}", cause)

class CallSiteConversionException(cause: Throwable, uri: String, synthetic: s.Synthetic)
    extends Exception(s"Unable to convert $synthetic ($uri}: ${cause.getMessage}", cause)

class SkippedSymbolException(what: String) extends Exception(s"Skipped symbol $what")

class UnsupportedElementException[T](what: String, actual: T)
    extends Exception(s"Unsupported $what ($actual)")

class UnexpectedElementException[T](what: String, actual: T)
    extends Exception(s"Unexpected $what ($actual)")

case class MissingSymbolException(message: String) extends Exception(message)

class LoadingMetadataException(message: String) extends Exception(message)

case class MissingImplicitArguments(code: String, declarationId: String)
    extends Exception(s"$code -- $declarationId")

case class UnExtractableCallSiteException(synthetic: s.Synthetic, uri: String)
    extends Exception(
      s"Unable to extract ${synthetic.tree} into a call site $uri:${synthetic.range.getOrElse("???")}"
    )
