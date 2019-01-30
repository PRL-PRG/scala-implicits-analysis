package cz.cvut.fit.prl.scala.implicits.extractor

import cz.cvut.fit.prl.scala.implicits.model.Location
import cz.cvut.fit.prl.scala.implicits.utils._

import scala.meta.internal.{semanticdb => s}

object SemanticdbSymbolResolver {

  def apply(
      dbs: Seq[s.TextDocument],
      symtab: SymbolTable,
      sourcePaths: List[String]): SemanticdbSymbolResolver = {

    def sourcePath(uri: String) =
      sourcePaths
        .collectFirst { case x if uri.startsWith(x) => x }
        .getOrElse("")

    /** all symbols that are defined and exists in the given db */
    val localSymbols = {
      for {
        db <- dbs
        path = sourcePath(db.uri)
        relativeUri = db.uri.substring(path.length)
        s.SymbolOccurrence(rangeOpt, symbol, s.SymbolOccurrence.Role.DEFINITION) <- db.occurrences
        symbolInfo <- db.symbols.find(_.symbol == symbol)
        range = rangeOpt.map(_.toLocal)
      } yield
        symbolInfo.symbol -> ResolvedSymbol(symbolInfo, Some(Location(path, relativeUri, range)))
    }.toMap

    val localRangeIndex = {
      for {
        db <- dbs
        s.SymbolOccurrence(Some(range), symbol, _) <- db.occurrences
      } yield (db.uri -> range) -> symbol
    }.toMap

    new SemanticdbSymbolResolver(localSymbols, localRangeIndex, symtab, dbs)
  }
}

class SemanticdbSymbolResolver(
    localSymbolIndex: Map[String, ResolvedSymbol],
    localRangeIndex: Map[(String, s.Range), String],
    symtab: SymbolTable,
    dbs: Seq[s.TextDocument])
    extends SymbolResolver {

  override def resolveSymbol(unit: String, range: s.Range): ResolvedSymbol = {
    localRangeIndex
      .get((unit, range))
      .map(resolveSymbol)
      .getOrThrow(MissingSymbolException(s"at $unit:$range"))
  }

  override def resolveSymbol(name: String): ResolvedSymbol = {
    val symbol = localSymbolIndex.get(name).orElse(symtab.resolve(name))

    symbol.getOrThrow({
      val e = MissingSymbolException(s"symbol: $name")
      e
    })
  }
}
