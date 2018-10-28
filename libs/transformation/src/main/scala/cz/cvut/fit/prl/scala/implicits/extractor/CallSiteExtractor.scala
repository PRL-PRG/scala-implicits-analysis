package cz.cvut.fit.prl.scala.implicits.extractor

import com.typesafe.scalalogging.LazyLogging
import cz.cvut.fit.prl.scala.implicits.utils._
import cz.cvut.fit.prl.scala.implicits.{model => m}
import cz.cvut.fit.prl.scala.implicits.model._

import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.{semanticdb => s}
import scala.meta._
import scala.meta.internal.semanticdb.SymbolOccurrence.Role.REFERENCE
import scala.meta.internal.semanticdb.TreeMessage.SealedValue.OriginalTree
import scala.util.{Failure, Success, Try}

class CallSiteExtractor(ctx: ExtractionContext) {

  implicit val _ctx = ctx

  class Converter(db: s.TextDocument, terms: Map[s.Range, Term]) {

    def findFunctionTerm(t: Term): Option[Tree] = t match {
      case Term.Select(_, name)       => Some(name)
      case x: Term.Name               => Some(x)
      case Term.ApplyType(fun, _)     => findFunctionTerm(fun)
      case Term.Apply(fun, _)         => findFunctionTerm(fun)
      case Term.New(Init(_, name, _)) => Some(name)
      case _                          => None
    }

    case class Argument(declaration: m.Declaration, typeArguments: List[m.Type]) {

      def toTypeRef: m.TypeRef =
        m.TypeRef(declaration.ref, typeArguments)
    }

    // arguments are inversed - the implicit ones are on top
    case class Call(
        declaration: m.Declaration,
        code: String = "",
        location: m.Location = m.Location.Empty,
        typeArguments: List[m.Type] = Nil,
        arguments: List[List[Argument]] = Nil) {

      def isImplicit: Boolean =
        declaration.isImplicit || arguments.flatten.exists(_.declaration.isImplicit)

      def toCallSite: m.CallSite = {
        val implicitArgumentsTypes: Seq[m.TypeRef] =
          if (declaration.hasImplicitParameters) arguments.head.map(_.toTypeRef)
          else Seq()

        m.CallSite(
          declaration.ref,
          code,
          location,
          typeArguments,
          implicitArgumentsTypes
        )
      }
    }

    def createArguments(tree: s.Tree): Option[Argument] = tree match {
      case s.ApplyTree(fn, _) => createArguments(fn)
      case s.TypeApplyTree(fn, args) =>
        createArguments(fn).map(_.copy(typeArguments = args.toList.map(ctx.createType)))
      case s.FunctionTree(_, body) =>
        createArguments(body)
      case s.SelectTree(_, Some(id)) =>
        createArguments(id)
      case s.IdTree(symbol) if !symbol.isLocal =>
        val declaration = ctx.resolveDeclaration(symbol)
        Some(Argument(declaration, Nil))
      case _ => None
    }

    def convert(synthetic: s.Synthetic): List[m.CallSite] = {

      val syntheticLocation = Local(db.uri, synthetic.range.get)

      // TODO: it should be enough to have a flag, or maybe even not that
      def convertInternal(tree: s.Tree, inCall: Boolean): List[Call] = tree match {

        case s.ApplyTree(fn, arguments) => {
          val callSitesFromArguments = arguments.flatMap(x => convertInternal(x, false))
          (convertInternal(fn, true) match {
            case cs :: css =>
              val args = arguments.toList.flatMap(createArguments)
              cs.copy(arguments = args :: cs.arguments) :: css
            case css => css
          }) ++ callSitesFromArguments

        }

        case s.SelectTree(qualifier, Some(s.IdTree(symbol))) => {
          val declaration = ctx.resolveDeclaration(symbol)
          val code = s".${declaration.name}"
          val cs = Call(declaration, code, location = syntheticLocation)

          qualifier match {
            case s.OriginalTree(Some(range)) =>
              val location = m.Local(db.uri, range)
              cs.copy(location = location) :: Nil
            case _ =>
              cs :: convertInternal(qualifier, false)
          }
        }

        case s.TypeApplyTree(fn, args) => {
          convertInternal(fn, true) match {
            case cs :: css =>
              val typeArguments = args.toList.map(ctx.createType)
              cs.copy(
                code = cs.code + typeArguments.map(_.asCode).mkString("[", ", ", "]"),
                typeArguments = typeArguments
              ) :: css
            case css => css
          }
        }

        case s.FunctionTree(_, body) => convertInternal(body, inCall)

        case s.OriginalTree(Some(range)) => {
          // TODO: refactor
          val csOpt = for {
            term <- terms.get(range)
            fnTerm <- findFunctionTerm(term)
          } yield {
            // check if there is some inferred method call e.g.
            // Future(1) ==> Future.apply(1)
            // we want ot get a reference to actual methods, not objects
            db.synthetics.find(_.range.exists(_ == fnTerm.pos.toRange)).flatMap(x => convertInternal(x.tree, false).headOption)
              .getOrElse {
                val symbol = db.occurrences.collectFirst {
                  case s.SymbolOccurrence(Some(sr), s, _) if sr == fnTerm.pos.toRange =>
                    s
                }.getOrThrow(
                    MissingSymbolException(s"Missing symbol for $term at $range in ${db.uri}"))

                val declaration = ctx.resolveDeclaration(symbol)
                val location = m.Local(db.uri, range)
                val code = declaration.name
                Call(declaration, code, location)
              }
          }

          csOpt.toList
        }

        case s.IdTree(symbol) if inCall => {
          val declaration = ctx.resolveDeclaration(symbol)
          val code = declaration.name

          Call(declaration, code, location = syntheticLocation) :: Nil
        }

        case _ => Nil
      }

      val intermediate = convertInternal(synthetic.tree, false)
      intermediate.filter(_.isImplicit).map(_.toCallSite)
    }
  }

  def extractImplicitCallSites(db: s.TextDocument): Seq[Try[m.CallSite]] = {
    val ast = db.text.parse[Source].get
    val terms = ast.collect {
      case x: Term => x.pos.toRange -> x
    }.toMap

    val converter = new Converter(db, terms)
    db.synthetics.toList
      .map(x => x -> Try(converter.convert(x)))
      .collect {
        case (_, Success(xs)) => xs.filter(_.isImplicit).map(Success(_))
        case (synthetic, Failure(x)) =>
          List(Failure(CallSiteConversionException(x, db.uri, synthetic)))
      }
      .flatten
  }

  def callSiteCount(db: s.TextDocument): Int = {
    def process(n: Int)(tree: Tree): Int =
      tree match {
        case Term.Apply(_, args) =>
          n + 1 + args.map(process(0)).sum
        case Term.ApplyType(fun, _) =>
          n + 1 + process(0)(fun)

        case Term.Select(qual, _) =>
          n + 1 + process(0)(qual)

        case Term.ApplyInfix(lhs, _, _, args) =>
          n + 1 + process(0)(lhs) + args.map(process(0)).sum

        case Term.ApplyUnary(op, arg) =>
          n + 1 + process(0)(arg)

        case Term.Interpolate(prefix, _, args) =>
          n + 1 + args.map(process(0)).sum

        case Term.New(Init(tpe, name, argss)) =>
          n + 1 + argss.flatMap(x => x.map(process(0))).sum

        // TODO New.Anonymous

        case Pkg(_, stats) =>
          n + stats.map(process(0)).sum
        case Ctor.Primary(_, _, paramss) =>
          n + paramss.flatten.map(process(0)).sum
        case Ctor.Secondary(_, _, paramss, _, stats) =>
          n + paramss.flatten.map(process(0)).sum + stats.map(process(0)).sum
        case Term.Param(_, _, _, Some(default)) =>
          n + process(0)(default)
        case Term.Param(_, _, _, None) =>
          n
        case Defn.Trait(_, _, _, ctor, templ) =>
          n + process(0)(ctor) + process(0)(templ)
        case Defn.Class(_, _, _, ctor, templ) =>
          n + process(0)(ctor) + process(0)(templ)
        case Defn.Object(_, _, templ) =>
          n + process(0)(templ)
        case Defn.Var(_, _, _, Some(rhs)) =>
          n + process(0)(rhs)
        case Defn.Var(_, _, _, None) =>
          n
        case Defn.Val(_, _, _, rhs) =>
          process(n)(rhs)
        case Defn.Def(_, _, _, paramss, _, body) =>
          n + paramss.flatten.map(process(0)).sum + process(0)(body)
        case _ @(_: Defn.Type | _: Import) =>
          n
        case t =>
          n + t.children.map(process(0)).sum
      }

    val ast = db.text.parse[Source].get
    process(0)(ast)
  }

}
