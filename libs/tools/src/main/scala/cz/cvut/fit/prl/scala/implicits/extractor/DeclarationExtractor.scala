package cz.cvut.fit.prl.scala.implicits.extractor

import cz.cvut.fit.prl.scala.implicits.utils._
import cz.cvut.fit.prl.scala.implicits.{model => m}

import scala.meta.internal.{semanticdb => s}
import scala.meta.internal.semanticdb.Scala._
import scala.util.{Failure, Success, Try}

class DeclarationExtractor(ctx: ExtractionContext) {

  implicit private val _ctx: ExtractionContext = ctx

  def extractImplicitDeclarations(db: s.TextDocument): Seq[Try[m.Declaration]] = {
    def position(symbolInfo: s.SymbolInformation): Option[s.Range] =
      db.occurrences
        .find(x => x.symbol == symbolInfo.symbol && x.role.isDefinition)
        .flatMap(_.range)

    db.symbols
      .filter(x => x.isImplicit)
      .map(x => x -> Try(extract(x)(db)))
      .collect {
        case (_, Success(Some(x))) => Success(x)
        case (symbolInfo, Failure(x)) =>
          Failure(new DeclarationConversionException(x, db.uri, position(symbolInfo), symbolInfo))
      }
  }

  private def extract(symbolInfo: s.SymbolInformation)(implicit db: s.TextDocument): Option[m.Declaration] =
    symbolInfo match {
      case x if x.isParameter && !x.symbol.isLocal =>
        val parent = x.parent

        if (parent.isMethod && !parent.isImplicit) {
          Some(ctx.resolveDeclaration(parent.symbol))
        } else {
          None
        }
      case x if x.isClass || x.isMethod || x.isObject || x.isField || x.isMacro || x.isParameter || x.isLocal =>
        Some(ctx.resolveDeclaration(symbolInfo.symbol))
      case x =>
        throw new SkippedSymbolException(x.toString)
    }
}
