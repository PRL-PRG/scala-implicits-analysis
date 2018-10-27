package cz.cvut.fit.prl.scala.implicits.extractor

import com.typesafe.scalalogging.LazyLogging
import cz.cvut.fit.prl.scala.implicits.utils._
import cz.cvut.fit.prl.scala.implicits.{model => m}
import cz.cvut.fit.prl.scala.implicits.model._
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.{semanticdb => s}
import scala.meta._
import scala.meta.internal.semanticdb.TreeMessage.SealedValue.OriginalTree
import scala.util.{Failure, Success, Try}

class CallSiteExtractor(ctx: ExtractionContext) {

  implicit val _ctx = ctx

  class Converter(db: s.TextDocument, terms: Map[s.Range, Term]) {

    def findFunctionTerm(t: Term): Option[Term.Name] = t match {
      case Term.Select(_, name)   => Some(name)
      case x: Term.Name           => Some(x)
      case Term.ApplyType(fun, _) => findFunctionTerm(fun)
      case Term.Apply(fun, _)     => findFunctionTerm(fun)
      case _                      => None
    }

    def convert(tree: s.Tree, callSites: List[m.CallSite]): List[m.CallSite] = tree match {

      case s.ApplyTree(fn, arguments) => {
        (convert(fn, Nil) match {
          case cs :: css =>
            arguments match {
              case Seq(s.OriginalTree(Some(range))) =>
                val code = cs.code + s"(${terms(range)})"
                val location = m.Local(db.uri, range)

                cs.copy(code=code, location=location) :: css
              case _ =>
                val nested = arguments.toList.flatMap(x => convert(x, Nil))
                val callSitesFromArguments = nested.filter(_.ref.declaration.isMethod)
                val args = nested.filter(_.ref.declaration.isImplicit)
                val csWithArgs = cs.copy(implicitArgumentTypes = args.map(x =>
                  m.TypeRef(x.ref, x.typeArguments)
                ))
                csWithArgs :: callSitesFromArguments ++ css
            }
          case css => css
        }) ++ callSites
      }

      case s.SelectTree(qualifier, Some(s.IdTree(symbol))) => {
        val declaration = ctx.resolveDeclaration(symbol)
        val code = s".${declaration.name}"
        // TODO: location
        val cs = m.CallSite(declaration.ref, code, m.Location.Empty)

        qualifier match {
          case s.OriginalTree(Some(range)) =>
            val code = s"(${terms(range)}).${declaration.name}"
            val location = m.Local(db.uri, range)
            cs.copy(code = code, location = location) :: callSites
          case _ =>
            cs :: convert(qualifier, callSites)
        }
      }

      case s.TypeApplyTree(fn, args) => {
        convert(fn, callSites) match {
          case cs :: css => cs.copy(typeArguments = args.map(ctx.createType)) :: css
          case css       => css
        }
      }

      case s.FunctionTree(_, body) => convert(body, callSites)

      case s.OriginalTree(Some(range)) => {
        val term = terms(range)
        findFunctionTerm(term)
          .map { fnTerm =>
            val symbol = db.occurrences
              .collectFirst {
                case s.SymbolOccurrence(Some(sr), s, _) if sr == fnTerm.pos.toRange => s
              }
              .getOrThrow(MissingSymbolException(s"Missing symbol for $term at $range in ${db.uri}"))

            val declaration = ctx.resolveDeclaration(symbol)
            val code = terms(range).toString()

            m.CallSite(declaration.ref, code, m.Local(db.uri, range)) :: callSites
          }.getOrElse(callSites)
      }

      case s.IdTree(symbol) => {
        val declaration = ctx.resolveDeclaration(symbol)
        val code = s"${declaration.name}"
        m.CallSite(declaration.ref, code, m.Location.Empty) :: Nil
      }

      case _ => callSites
    }
  }

  def extractCallSites(db: s.TextDocument): Seq[Try[m.CallSite]] = {
    val ast = db.text.parse[Source].get
    val terms = ast.collect {
      case x: Term => x.pos.toRange -> x
    }.toMap

    val converter = new Converter(db, terms)
    db.synthetics.toList
      .map(x => x -> Try(converter.convert(x.tree, Nil)))
      .collect {
        case (_, Success(xs)) => xs.filter(_.isImplicit).map(Success(_))
        case (synthetic, Failure(x)) =>
          List(Failure(CallSiteConversionException(x, db.uri, synthetic)))
      }
      .flatten
  }
}
