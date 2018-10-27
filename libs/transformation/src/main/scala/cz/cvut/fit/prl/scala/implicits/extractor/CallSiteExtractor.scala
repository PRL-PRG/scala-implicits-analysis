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

//    def resolveDeclaration(that: s.Tree): m.Declaration = that match {
//      case s.ApplyTree(fn, _)          => resolveDeclaration(fn)
//      case s.TypeApplyTree(fn, _)      => resolveDeclaration(fn)
//      case s.IdTree(symbol)            => ctx.resolveDeclaration(symbol)
//      case s.SelectTree(qualifier, id) => resolveDeclaration(qualifier)
//      case s.OriginalTree(Some(range)) => {
//        val term = terms(range)
//        val fnTerm = findFunctionTerm(term)
//        val symbol = db.occurrences
//          .collectFirst {
//            case s.SymbolOccurrence(Some(sr), s, _) if sr == fnTerm.pos.toRange => s
//          }
//          .getOrThrow(MissingSymbolException(s"Missing symbol for $term at $range in ${db.uri}"))
//
//        ctx.resolveDeclaration(symbol)
//      }
//      case _ => throw UnsupportedElementException("Synthetic Function", that)
//    }

    def resolveTypeArguments(that: s.Tree): Seq[m.Type] = that match {
      case s.ApplyTree(fn, _)          => resolveTypeArguments(fn)
      case s.TypeApplyTree(_, args)    => args.map(ctx.createType)
      case s.SelectTree(qualifier, id) => resolveTypeArguments(qualifier)
      case s.OriginalTree(_)           => Seq()
      case s.IdTree(_)                 => Seq()
      case _                           => throw UnsupportedElementException("Synthetic TypeArguments", that)
    }

//    def createCallSite(tree: s.Tree): m.CallSite = tree match {
//      case s.OriginalTree(Some(range)) => {
//        val term = terms(range)
//        val fnTerm = findFunctionTerm(term)
//        val symbol = db.occurrences
//          .collectFirst {
//            case s.SymbolOccurrence(Some(sr), s, _) if sr == fnTerm.pos.toRange => s
//          }
//          .getOrThrow(MissingSymbolException(s"Missing symbol for $term at $range in ${db.uri}"))
//
//        val declaration = ctx.resolveDeclaration(symbol)
//        val code = terms(range).toString()
//
//        m.CallSite(declaration.ref, code, m.Local(db.uri, range))
//      }
//
//      case s.SelectTree(s.OriginalTree(Some(range)), Some(s.IdTree(symbol))) => {
//        val declaration = ctx.resolveDeclaration(symbol)
//        val code = s"(${terms(range)}).${declaration.name}"
//
//        m.CallSite(declaration.ref, code, m.Local(db.uri, range))
//      }
//
//      case s.TypeApplyTree(fn, args) => {
//        val cs = createCallSite(fn)
//        cs.copy(typeArguments = args.map(ctx.createType))
//      }
//
//      case s.IdTree(symbol) => {
//        val declaration = ctx.resolveDeclaration(symbol)
//        val code = s"${declaration.name}"
//        m.CallSite(declaration.ref, code, m.Location.Empty)
//      }
//
//      case x =>
//        throw UnexpectedElementException("Synthetic TypeArguments", x)
//    }

    def convert(tree: s.Tree, callSites: List[m.CallSite]): List[m.CallSite] = tree match {
      // implicit conversion
//      case s.ApplyTree(fn, Seq(s.OriginalTree(Some(range)))) => {
//        val cs = convert(fn, Nil).headOption.getOrElse(createCallSite(fn))
//        val code = cs.code + s"(${terms(range)})"
//
//        cs.copy(code = code, location = m.Local(db.uri, range)) :: callSites
//      }

      // parenthesis - adding an argument list to a function call
      case s.ApplyTree(fn, arguments) => {
        convert(fn, callSites) match {
          case cs :: css =>
            arguments match {
              case Seq(s.OriginalTree(Some(range))) =>
                val code = cs.code + s"(${terms(range)})"
                val location = m.Local(db.uri, range)

                cs.copy(code=code, location=location) :: css
              case _ =>
                val nested = arguments.toList.flatMap(x => convert(x, css))
                val callSitesFromArguments = nested.filter(_.ref.declaration.isMethod)
                val args = nested.filter(_.ref.declaration.isImplicit)
                val csWithArgs = cs.copy(implicitArgumentTypes = args.map(x =>
                  m.TypeRef(x.ref, x.typeArguments)
                ))
                csWithArgs :: callSitesFromArguments
            }
          case css => css
        }
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

            m.CallSite(declaration.ref, code, m.Local(db.uri, range)) :: Nil
          }
          .getOrElse(Nil)
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
        case (_, Success(xs)) => xs.map(Success(_))
        case (synthetic, Failure(x)) =>
          List(Failure(CallSiteConversionException(x, db.uri, synthetic)))
      }
      .flatten
  }
}
