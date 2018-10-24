package cz.cvut.fit.prl.scala.implicits.extractor
import cz.cvut.fit.prl.scala.implicits.symtab.ResolvedSymbol

import scala.meta.internal.{semanticdb => s}

trait SymbolResolver {
  def resolveSymbol(symbol: String): ResolvedSymbol
  def resolveSymbol(unit: String, range: s.Range): ResolvedSymbol
}
