package cz.cvut.fit.prl.scala.implicits.extractor

import cz.cvut.fit.prl.scala.implicits.model.Location
import cz.cvut.fit.prl.scala.implicits.utils._

import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.semanticdb.SymbolOccurrence
import scala.meta.internal.{semanticdb => s}

object SemanticdbSymbolResolver {

  def apply(dbs: Seq[s.TextDocument], symtab: SymbolTable): SemanticdbSymbolResolver = {
    val localSymbols = {
      for {
        db <- dbs
        symbolInfo <- db.symbols if symbolInfo.symbol.isLocal
      } yield {
        val range = db.occurrences.collectFirst {
          case SymbolOccurrence(
              Some(range),
              symbolInfo.symbol,
              s.SymbolOccurrence.Role.DEFINITION) =>
            range.toLocal
        }
        symbolInfo.symbol -> ResolvedSymbol(symbolInfo, Some(Location(db.uri, range)))
      }
    }.toMap

    val localRangeIndex = {
      for {
        db <- dbs
        s.SymbolOccurrence(Some(range), symbol, _) <- db.occurrences
      } yield (db.uri -> range) -> symbol
    }.toMap

    new SemanticdbSymbolResolver(localSymbols, localRangeIndex, symtab)
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

  override def resolveSymbol(name: String): ResolvedSymbol = {
    val symbol = if (name.isLocal) localSymbolIndex.get(name) else symtab.resolve(name)
    symbol.getOrThrow({
      val e = MissingSymbolException(s"symbol: $name")
      e
    })
  }
}
