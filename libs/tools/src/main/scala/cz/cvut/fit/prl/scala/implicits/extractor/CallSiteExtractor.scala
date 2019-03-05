package cz.cvut.fit.prl.scala.implicits.extractor

import cz.cvut.fit.prl.scala.implicits.model._
import cz.cvut.fit.prl.scala.implicits.utils._

import scala.collection.mutable
import scala.meta._
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.{semanticdb => s}
import scala.util.{Failure, Success, Try}

class CallSiteExtractor(ctx: ExtractionContext) {

  implicit val _ctx = ctx

  class Converter(moduleId: String, db: s.TextDocument, terms: Map[s.Range, Term]) {

    def findFunctionTerm(t: Term): Option[Tree] = t match {
      case Term.Select(_, _) => Some(t)
      case Term.Name(_) => Some(t)
      case Term.ApplyType(fun, _) => findFunctionTerm(fun)
      case Term.Apply(fun, _) => findFunctionTerm(fun)
      case Term.New(Init(_, name, _)) => Some(name)
      case Term.ApplyUnary(op, _) => Some(op)
      case Term.ApplyInfix(_, op, _, _) => Some(op)
      case _ => None
    }

    def processSynthetic(synthetic: s.Synthetic): Iterable[CallSite] = {
      val path = ctx.relaxedSourcePath(db.uri)
      val relativeUri = db.uri.substring(path.length)
      val syntheticLocation = Location(path, relativeUri, Some(synthetic.range.get))

      val extractedCallSites = mutable.Map[Int, CallSite]()
      val extractedCallSiteArguments =
        mutable.Map[Int, List[List[Argument]]]().withDefaultValue(Nil)

      // creates either a ValueRef or a CallSiteRef if extractedArgumentCallSite is defined
      // and the tree is eventually resolves to a method
      def createArgument(
          callSite: CallSite,
          tree: s.Tree,
          extractedArgumentCallSite: Option[CallSite]): Option[Argument] = tree match {
        case s.ApplyTree(fn, _) =>
          createArgument(callSite, fn, extractedArgumentCallSite)
        case s.TypeApplyTree(fn, _) =>
          // we do not need to care about type arguments since if there were any
          // they will be already part of the call site in extractedArgumentCallSite
          createArgument(callSite, fn, extractedArgumentCallSite)
        case s.FunctionTree(_, body) =>
          createArgument(callSite, body, extractedArgumentCallSite)
        case s.SelectTree(_, Some(id)) =>
          createArgument(callSite, id, extractedArgumentCallSite)
        case s.IdTree(symbol) if !symbol.isLocal =>
          val declaration = ctx.resolveDeclaration(symbol)
          (declaration.isMethod, extractedArgumentCallSite) match {
            case (true, Some(cs)) =>
              extractedCallSites
                .updateValue(cs.callSiteId, _.copy(parentId = Some(callSite.callSiteId)))
              Some(CallSiteRef(cs.callSiteId))
            case (false, None) =>
              Some(ValueRef(declaration.declarationId))
            case _ =>
              throw new Exception(
                s"Invalid argument $declaration and argument call site ($extractedArgumentCallSite)")
          }
        case s.IdTree(symbol) =>
          None
        case s.MacroExpansionTree(beforeExpansion, tpe) =>
          createArgument(callSite, beforeExpansion, extractedArgumentCallSite)
        case s.OriginalTree(_) =>
          None
        case s.Tree.Empty =>
          None
      }

      def createCallSite(declaration: Declaration, code: String, location: Location): CallSite = {
        val id = ctx.createCallSiteId
        val cs = CallSite(id, None, moduleId, declaration.declarationId, code, location)
        extractedCallSites += id -> cs
        cs
      }

      // inCall signals if we are resolving from children of ApplyTree
      def convertInternal(tree: s.Tree, inCall: Boolean): Option[CallSite] = tree match {

        case s.ApplyTree(fn, argumentsTree) => {

          val callSitesFromArguments = argumentsTree.map(x => convertInternal(x, false))

          convertInternal(fn, true)
            .map { callSite =>
              argumentsTree match {
                case Seq(s.OriginalTree(Some(range))) =>
                  // implicit conversion
                  extractedCallSites.updateValue(
                    callSite.callSiteId,
                    _.copy(code = callSite.code + terms.get(range).map(x => s"($x)").getOrElse(""))
                  )
                case _ =>
                  val arguments =
                    argumentsTree
                      .zip(callSitesFromArguments)
                      .map(x => createArgument(callSite, x._1, x._2))
                      .collect { case Some(x) => x }
                      .toList

                  if (arguments.nonEmpty) {
                    extractedCallSiteArguments.updateValue(callSite.callSiteId, arguments :: _)
                  }
              }

              callSite
            }
            .orElse {
              throw new Exception(s"Unable to convert: $tree into a call site")
            }
        }

        case s.SelectTree(qualifier, Some(s.IdTree(symbol))) => {
          if (!inCall && qualifier.isInstanceOf[s.OriginalTree]) {
            // this hack is here because we do not want to create
            // implicit calls from inferred method calls like .apply on
            // companion objects
            None
          } else {
            val declaration = ctx.resolveDeclaration(symbol)
            val code = s".${declaration.name}"
            val cs = createCallSite(declaration, code, syntheticLocation)

            qualifier match {
              case s.OriginalTree(Some(range)) =>
                val location = Location(path, relativeUri, Some(range))
                Some(extractedCallSites.updateValue(cs.callSiteId, _.copy(location = location)))
              case _ =>
                convertInternal(qualifier, false)
                Some(cs)
            }
          }
        }

        case s.TypeApplyTree(fn, args) => {
          convertInternal(fn, inCall).map { cs =>
            val typeArguments = ctx.createTypeArguments(args, includeTopBottom = true)
            extractedCallSites.updateValue(
              cs.callSiteId,
              _.copy(
                typeArguments = typeArguments,
                code = cs.code + typeArguments.map(_.asCode).mkString("[", ", ", "]")
              )
            )
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
              .flatMap(x => convertInternal(x.tree, true))
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
                val location = Location(path, relativeUri, Some(range))
                val code = declaration.name
                createCallSite(declaration, code, location)
              }
          }

          csOpt
        }

        case s.IdTree(symbol) => {
          val declaration = ctx.resolveDeclaration(symbol)
          // TODO: this should be only for the case of implicit parameters
          if (inCall || (declaration.isImplicit && declaration.isFunctionLike)) {
            val code = declaration.name

            Some(createCallSite(declaration, code, syntheticLocation))
          } else {
            None
          }
        }

        case _ => None
      }

      convertInternal(synthetic.tree, false)

      extractedCallSites.values.map { cs =>
        val takesImplicitParams = cs.declaration.hasImplicitParameters

        val implicitArgumentsTypes = extractedCallSiteArguments.get(cs.callSiteId) match {
          case Some(argumentsLists) if takesImplicitParams =>
            argumentsLists.head
          case None if !takesImplicitParams =>
            Seq.empty
          case _ =>
            throw new UnexpectedElementException(
              "Callsite declaration",
              s"${cs.code} -- ${cs.declarationId}")
        }

        cs.copy(implicitArgumentTypes = implicitArgumentsTypes)
      }
    }
  }

  def extractImplicitCallSites(
      moduleId: String,
      db: s.TextDocument,
      ast: Source): Iterable[Try[CallSite]] = {

    def rangeFromStartToEndNodes(start: Tree, end: Tree): s.Range =
        s.Range(
          start.pos.startLine, start.pos.startColumn,
          end.pos.endLine, end.pos.endColumn
        )

    val terms = ast.collect {
      // an expression such as (a + b) will have the Term.pos including the parens
      // while the OriginalTree from semanticdb will have it without
      // so we just "unbox" it using op and arg
      case t @ Term.ApplyInfix(lhs, _, _, args) =>
        rangeFromStartToEndNodes(lhs, args.last) -> t

      // an expression such as (-a) will have the Term.pos including the parens
      // while the OriginalTree from semanticdb will have it without
      // so we just "unbox" it using op and arg
      case t @ Term.ApplyUnary(op, arg) =>
        rangeFromStartToEndNodes(op, arg) -> t

      case t: Term => t.pos.toRange -> t
    }.toMap

    val converter = new Converter(moduleId, db, terms)
    val result = db.synthetics.toList
      .map(x => x -> Try(converter.processSynthetic(x)))
      .collect {
        case (_, Success(xs)) =>
          xs.filter(_.isImplicit).map(Success(_))
        case (synthetic, Failure(x)) =>
          List(Failure(new CallSiteConversionException(x, db.uri, synthetic)))
      }
      .flatten

    result
  }

  def callSiteCount(ast: Source): Int = {
    def process(n: Int)(tree: Tree): Int =
      tree match {
        case Term.Apply(Term.Select(qual, _), args) =>
          n + 1 + args.map(process(0)).sum + process(0)(qual)
        case Term.Apply(fun: Term.Apply, args) =>
          // fun(1)(2)(3) should be one call
          n + args.map(process(0)).sum + process(0)(fun)
        case Term.Apply(fun, args) =>
          n + 1 + args.map(process(0)).sum + process(0)(fun)
        case Term.ApplyType(Term.Select(qual, _), _) =>
          n + 1 + process(0)(qual)
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

        // covers Term.NewAnonymous and Term.New
        case Init(tpe, name, argss) =>
          n + 1 + argss.flatMap(x => x.map(process(0))).sum

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
