package cz.cvut.fit.prl.scala.implicits.extractor

import scala.meta.internal.{semanticdb => s}

trait SymbolResolver {
  def resolveSymbol(name: String)(implicit db: s.TextDocument): ResolvedSymbol
  def resolveSymbol(range: s.Range)(implicit db: s.TextDocument): ResolvedSymbol
}
