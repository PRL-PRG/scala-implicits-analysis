package cz.cvut.fit.prl.scala.implicits.extractor

import cz.cvut.fit.prl.scala.implicits.model._
import cz.cvut.fit.prl.scala.implicits.utils._
import cz.cvut.fit.prl.scala.implicits.{ModuleMetadata, model => m}

import scala.meta._
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.{semanticdb => s}
import scala.util.{Failure, Success, Try}

class CallSiteExtractor(ctx: ExtractionContext) {

  implicit val _ctx = ctx

  class Converter(moduleId: String, db: s.TextDocument, terms: Map[s.Range, Term]) {

    def findFunctionTerm(t: Term): Option[Tree] = t match {
      case Term.Select(_, name) => Some(t)
      case Term.Name(_) => Some(t)
      case Term.ApplyType(fun, _) => findFunctionTerm(fun)
      case Term.Apply(fun, _) => findFunctionTerm(fun)
      case Term.New(Init(_, name, _)) => Some(name)
      case _ => None
    }

    case class Argument(declaration: m.Declaration, typeArguments: List[m.Type]) {

      def toTypeRef: m.TypeRef =
        m.TypeRef(declaration.declarationId, typeArguments)
    }

    // arguments are inversed - the implicit ones are on top
    case class Call(
        declaration: m.Declaration,
        code: String = "",
        location: m.Location,
        typeArguments: List[m.Type] = Nil,
        argumentss: List[List[Argument]] = Nil) {

      def isImplicit: Boolean =
        declaration.isImplicit || declaration.hasImplicitParameters

      def toCallSite: m.CallSite = {
        val implicitArgumentsTypes: Seq[m.TypeRef] = {
          try {
            if (declaration.hasImplicitParameters) argumentss.head.map(_.toTypeRef) else Seq()
          } catch {
            case _: Throwable =>
              throw new UnexpectedElementException("Callsite declaration", s"$code -- $declaration")
          }
        }

        m.CallSite(
          moduleId,
          declaration.declarationId,
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
        createArguments(fn).map(_.copy(typeArguments = ctx.createTypeArguments(args)))
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

      val path = ctx.relaxedSourcePath(db.uri)
      val relativeUri = db.uri.substring(path.length)
      val syntheticLocation = Location(path, relativeUri, Some(synthetic.range.get))

      def convertInternal(tree: s.Tree, inCall: Boolean): List[Call] = tree match {

        case s.ApplyTree(fn, arguments) => {
          val callSitesFromArguments = arguments.flatMap(x => convertInternal(x, false))

          (convertInternal(fn, true) match {
            case cs :: css =>
              arguments match {
                case Seq(s.OriginalTree(Some(range))) =>
                  // implicit conversion
                  cs.copy(code = cs.code + terms.get(range).map(x => s"($x)").getOrElse("")) :: css
                case _ =>
                  val args = arguments.toList.flatMap(createArguments)
                  cs.copy(argumentss = args :: cs.argumentss) :: css
              }
            case css => css
          }) ++ callSitesFromArguments

        }

        case s.SelectTree(qualifier, Some(s.IdTree(symbol))) => {
          if (!inCall && qualifier.isInstanceOf[s.OriginalTree]) {
            // this hack is here because we do not want to create
            // implicit calls from inferred method calls like .apply on
            // companion objects
            Nil
          } else {
            val declaration = ctx.resolveDeclaration(symbol)
            val code = s".${declaration.name}"
            val cs = Call(declaration, code, location = syntheticLocation)

            qualifier match {
              case s.OriginalTree(Some(range)) =>
                val location = m.Location(path, relativeUri, Some(range))
                cs.copy(location = location) :: Nil
              case _ =>
                cs :: convertInternal(qualifier, false)
            }
          }
        }

        case s.TypeApplyTree(fn, args) => {
          convertInternal(fn, inCall) match {
            case cs :: css =>
              val typeArguments = ctx.createTypeArguments(args)
              cs.copy(
                code = cs.code + typeArguments.map(_.asCode).mkString("[", ", ", "]"),
                typeArguments = typeArguments
              ) :: css
            case css => css
          }
        }

        case s.FunctionTree(_, body) => convertInternal(body, inCall)

        case s.OriginalTree(Some(range)) if inCall => {
          // TODO: refactor - what we need to is first resolve the inferred method calls and type parameters
          val csOpt = for {
            term <- terms.get(range)
            fnTerm <- findFunctionTerm(term)
          } yield {
            // check if there is some inferred method call e.g.
            // Future(1) ==> Future.apply(1)
            // we want ot get a reference to actual methods, not objects
            db.synthetics
              .find(term != fnTerm && _.range.exists(_ == fnTerm.pos.toRange))
              .flatMap(x => convertInternal(x.tree, true).headOption)
              .getOrElse {

                val nestedTermsToTry = fnTerm :: fnTerm.collect {
                  case Term.Select(_, name) => name
                }

                val occurrences =
                  for {
                    x <- nestedTermsToTry
                    y <- db.occurrences.find(_.range.exists(_ == x.pos.toRange))
                  } yield y

                val symbol = occurrences.headOption.map(_.symbol).getOrThrow {
                  val e =
                    MissingSymbolException(s"Missing function for $term at $range in ${db.uri}")
                  e
                }

                val declaration = ctx.resolveDeclaration(symbol)
                val location = m.Location(path, relativeUri, Some(range))
                val code = declaration.name
                Call(declaration, code, location)
              }
          }

          csOpt.toList
        }

        case s.IdTree(symbol) => {
          val declaration = ctx.resolveDeclaration(symbol)
          // TODO: this should be only for the case of implicit parameters
          if (inCall || (declaration.isImplicit && declaration.isFunctionLike)) {
            val code = declaration.name

            Call(declaration, code, location = syntheticLocation) :: Nil
          } else {
            Nil
          }
        }

        case _ => Nil
      }

      val intermediate = convertInternal(synthetic.tree, false)
      intermediate.filter(_.isImplicit).map(_.toCallSite)
    }
  }

  def extractImplicitCallSites(
      moduleId: String,
      db: s.TextDocument,
      ast: Source): Seq[Try[m.CallSite]] = {
    val terms = ast.collect {
      case x: Term => x.pos.toRange -> x
    }.toMap

    val converter = new Converter(moduleId, db, terms)
    db.synthetics.toList
      .map(x => x -> Try(converter.convert(x)))
      .collect {
        case (_, Success(xs)) => xs.filter(_.isImplicit).map(Success(_))
        case (synthetic, Failure(x)) =>
          List(Failure(new CallSiteConversionException(x, db.uri, synthetic)))
      }
      .flatten
  }

  def callSiteCount(ast: Source): Int = {
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

    process(0)(ast)
  }

}
