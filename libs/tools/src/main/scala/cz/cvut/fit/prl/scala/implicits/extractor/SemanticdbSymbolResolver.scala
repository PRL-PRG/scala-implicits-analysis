package cz.cvut.fit.prl.scala.implicits.extractor

import cz.cvut.fit.prl.scala.implicits.model.{Location, Position}
import cz.cvut.fit.prl.scala.implicits.utils._

import scala.collection.mutable
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.semanticdb.Language.SCALA
import scala.meta.internal.semanticdb.SymbolInformation.Kind.PARAMETER
import scala.meta.internal.semanticdb.SymbolOccurrence.Role.DEFINITION
import scala.meta.internal.{semanticdb => s}
import scala.util.matching.Regex

object SemanticdbSymbolResolver {

  val EvidenceParam: Regex = "evidence\\$\\d+".r

  private def sourcePathForUri(sourcePaths: List[String], uri: String) =
    sourcePaths
      .collectFirst { case x if uri.startsWith(x) => x }
      .getOrElse("")

  def apply(
      dbs: Seq[s.TextDocument],
      symtab: SymbolTable,
      sourcePaths: List[String]): SemanticdbSymbolResolver = {

    /** all symbols that are defined and exists in the given db except the local ones */
    val definitions = {
      for {
        db <- dbs
        path = sourcePathForUri(sourcePaths, db.uri)
        relativeUri = db.uri.substring(path.length)
        s.SymbolOccurrence(
          rangeOpt,
          symbol,
          DEFINITION
        ) <- db.occurrences if !symbol.isLocal
        symbolInfo <- db.symbols.find(_.symbol == symbol)
        range = rangeOpt.map(_.toPos)
      } yield {
        val resolvedSymbol = ResolvedSymbol(symbolInfo, Location(path, relativeUri, range))

        symbol -> resolvedSymbol
      }
    }.toMap

    new SemanticdbSymbolResolver(definitions, sourcePaths, symtab)
  }
}

class SemanticdbSymbolResolver(
    definitions: Map[String, ResolvedSymbol],
    sourcePaths: List[String],
    symtab: SymbolTable)
    extends SymbolResolver {

  import SemanticdbSymbolResolver._

  private val symbolCache = mutable.Map[(String, String), Option[ResolvedSymbol]]()

  override def resolveSymbol(range: s.Range)(implicit db: s.TextDocument): ResolvedSymbol = {
    db.occurrences
      .find(_.range.contains(range))
      .map(x => resolveSymbol(x.symbol))
      .getOrThrow(SymbolNotFoundException(s"at ${db.uri}:$range"))
  }

  override def resolveSymbol(name: String)(implicit db: s.TextDocument): ResolvedSymbol = {
    val symbol = if (name.isLocal) {
      val key = (db.uri, name)
      symbolCache.getOrElseUpdate(key, resolveLocalSymbol(name))
    } else {
      val key = ("", name)
      symbolCache.getOrElseUpdate(key, resolveGlobalSymbol(name).map(fixSymbol))
    }

    symbol.getOrThrow({
      val e = SymbolNotFoundException(s"symbol: $name")
      e
    })
  }

  private def resolveGlobalSymbol(name: String)(
      implicit db: s.TextDocument): Option[ResolvedSymbol] =
    definitions.get(name).orElse(symtab.resolve(name))

  private def resolveLocalSymbol(name: String)(
      implicit db: s.TextDocument): Option[ResolvedSymbol] =
    for {
      info <- db.symbols.find(_.symbol == name)
      path = sourcePathForUri(sourcePaths, db.uri)
      relativeUri = db.uri.substring(path.length)
      range = db.occurrences.find(x => x.symbol == name && x.role.isDefinition).flatMap(_.range)
    } yield ResolvedSymbol(info, Location(path, relativeUri, range.map(_.toPos)))

  private def fixSymbol(symbol: ResolvedSymbol)(implicit db: s.TextDocument): ResolvedSymbol =
    symbol match {
      case ResolvedSymbol(info, Location(path, uri, None)) =>
        ResolvedSymbol(info, Location(path, uri, fixPosition(symbol)))
      case _ =>
        symbol
    }

  private def fixPosition(symbol: ResolvedSymbol)(implicit db: s.TextDocument): Option[Position] = {
    assert(symbol.location.position.isEmpty)

    symbol.symbolInfo match {
      case si @ s.SymbolInformation(fqn, SCALA, PARAMETER, _, EvidenceParam(), _, _, _)
          if si.isImplicit =>
        for {
          ResolvedSymbol(
            owner,
            Location(_, _, Some(pos))
          ) <- definitions.get(fqn.owner) if owner.isMethod
        } yield pos.withStartCol(pos.endCol)
      case _ => None
    }
  }
}
