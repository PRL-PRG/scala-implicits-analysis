package cz.cvut.fit.prl.scala.implicits.extractor

import cz.cvut.fit.prl.scala.implicits.model.{Location, Position}
import cz.cvut.fit.prl.scala.implicits.utils._

import scala.collection.mutable
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.semanticdb.Language.SCALA
import scala.meta.internal.semanticdb.SymbolInformation.Kind.PARAMETER
import scala.meta.internal.{semanticdb => s}
import scala.util.matching.Regex

object SemanticdbSymbolResolver {

  val EvidenceParam: Regex = "evidence\\$\\d+".r

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
        range = rangeOpt.map(_.toPos)
      } yield symbolInfo.symbol -> ResolvedSymbol(symbolInfo, Location(path, relativeUri, range))
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

  import SemanticdbSymbolResolver._

  private val symbolCache = mutable.Map[String, ResolvedSymbol]()

  override def resolveSymbol(unit: String, range: s.Range): ResolvedSymbol = {
    localRangeIndex
      .get((unit, range))
      .map(resolveSymbol)
      .getOrThrow(MissingSymbolException(s"at $unit:$range"))
  }

  override def resolveSymbol(name: String): ResolvedSymbol = {
    val symbol = symbolCache.get(name).orElse {
      localSymbolIndex
        .get(name)
        .orElse(symtab.resolve(name))
        .map(updateCache)
    }

    symbol.getOrThrow({
      val e = MissingSymbolException(s"symbol: $name")
      e
    })
  }

  private def updateCache(symbol: ResolvedSymbol): ResolvedSymbol = {
    symbol match {
      case ResolvedSymbol(info, Location(path, uri, None)) =>
        val updated = ResolvedSymbol(info, Location(path, uri, fixPosition(symbol)))
        symbolCache += info.symbol -> updated
        updated
      case _ => symbol
    }
  }

  private def fixPosition(symbol: ResolvedSymbol): Option[Position] = {
    assert(symbol.location.position.isEmpty)

    symbol.symbolInfo match {
      case si @ s.SymbolInformation(fqn, SCALA, PARAMETER, _, EvidenceParam(), _, _, _)
          if si.isImplicit =>
        for {
          ResolvedSymbol(owner, loc @ Location(_, _, Some(pos))) <- localSymbolIndex.get(
            fqn.owner) if owner.isMethod
        } yield pos.withStartCol(pos.endCol)
      case _ => None
    }
  }
}
