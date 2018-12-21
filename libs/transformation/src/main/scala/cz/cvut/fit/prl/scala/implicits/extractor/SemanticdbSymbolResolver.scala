package cz.cvut.fit.prl.scala.implicits.extractor

import cz.cvut.fit.prl.scala.implicits.model.Location
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
        info <- db.symbols.find(_.symbol == symbol)
        location = Location(db.uri, Some(range))
        resolvedSymbol = ResolvedSymbol(info, Some(location))
      } yield symbol -> resolvedSymbol
    }.toMap

    def findSymbolLocation(o: s.SymbolInformation): Option[Location] = {
      def tryToFind(s: String): Option[Location] = localSymbols.get(s) match {
        case Some(ResolvedSymbol(_, location)) =>
          location
        case _ if s.isNone =>
          None
        case _ =>
          tryToFind(s.owner)
      }

      tryToFind(o.symbol.owner)
    }

    val localSyntheticSymbols = {
      for {
        db <- dbs
        info <- db.symbols if !localSymbols.contains(info.symbol)
        resolvedSymbol = ResolvedSymbol(info, findSymbolLocation(info))
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
      .getOrThrow({
        val e = MissingSymbolException(s"Unable to resolve: $name")
        e
      })

}
