package cz.cvut.fit.prl.scala.implicits.extractor

import cz.cvut.fit.prl.scala.implicits.model.{Local, SyntheticLocation}
import cz.cvut.fit.prl.scala.implicits.symtab.{ResolvedSymbol, SymbolTable}
import cz.cvut.fit.prl.scala.implicits.utils._

import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.{semanticdb => s}

object SemanticdbSymbolResolver {

  def apply(dbs: Seq[s.TextDocument], symtab: SymbolTable): SemanticdbSymbolResolver = {

    val localSymbols = {
      for {
        db <- dbs
        s.SymbolOccurrence(
          Some(range),
          symbol,
          s.SymbolOccurrence.Role.DEFINITION
        ) <- db.occurrences if !symbol.isPackage && !symbol.isLocal
        xx = db.symbols.find(_.symbol == symbol)
        info = xx.get
        location = Local(db.uri, range)
        resolvedSymbol = ResolvedSymbol(info, location)
      } yield symbol -> resolvedSymbol
    }.toMap

    val localSyntheticSymbols = {
      for {
        db <- dbs
        info <- db.symbols if !localSymbols.contains(info.symbol)
        resolvedSymbol = ResolvedSymbol(info, SyntheticLocation())
      } yield info.symbol -> resolvedSymbol
    }.toMap

    val localSymbolIndex = localSymbols ++ localSyntheticSymbols

    val localRangeIndex = {
      for {
        db <- dbs
        s.SymbolOccurrence(Some(range), symbol, _) <- db.occurrences
      } yield (db.uri -> range) -> symbol
    }.toMap

    new SemanticdbSymbolResolver(localSymbolIndex, localRangeIndex, symtab)
  }
}

class SemanticdbSymbolResolver(
    localSymbolIndex: Map[String, ResolvedSymbol],
    localRangeIndex: Map[(String, s.Range), String],
    symtab: SymbolTable)
    extends SymbolResolver {

  override def resolveSymbol(unit: String, range: s.Range): ResolvedSymbol = {
    localRangeIndex
      .get((unit, range))
      .map(resolveSymbol)
      .getOrThrow(MissingSymbolException(s"at $unit:$range"))
  }

  override def resolveSymbol(name: String): ResolvedSymbol =
    localSymbolIndex
      .get(name)
      .orElse(symtab.resolve(name))
      .getOrThrow(MissingSymbolException(name))

}
