package cz.cvut.fit.prl.scala.implicits.extractor

import cz.cvut.fit.prl.scala.implicits.model._
import cz.cvut.fit.prl.scala.implicits.utils._

import scala.collection.mutable
import scala.meta._
import scala.meta.inputs.{Position => MetaPos}
import scala.meta.internal.{semanticdb => s}
import scala.util.{Failure, Success, Try}

class CallSiteExtractor(ctx: ExtractionContext) {

  implicit val _ctx: ExtractionContext = ctx

  class Converter(moduleId: String, db: s.TextDocument, ast: Source) {

    implicit private val _db: s.TextDocument = db
    implicit private val declarationResolver: DeclarationResolver = (ref: DeclarationRef) =>
      ctx.resolveDeclaration(ref.declarationId)

    def processSynthetic(synthetic: s.Synthetic): Iterable[CallSite] = {
      val path = ctx.relaxedSourcePath(db.uri)
      val relativeUri = db.uri.substring(path.length)
      val syntheticLocation = Location(path, relativeUri, Some(synthetic.range.get.toPos))

      val extractedCallSites = mutable.Map[Int, CallSite]()
      val extractedCallSiteArgumentsLists =
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

        case s.SelectTree(_, None) =>
          None

        case s.IdTree(symbol) =>
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

        case s.MacroExpansionTree(beforeExpansion, _) =>
          createArgument(callSite, beforeExpansion, extractedArgumentCallSite)

        case s.LiteralTree(_) =>
          None

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

          val callSitesFromArguments = argumentsTree.map(x => convertInternal(x, inCall = false))

          Try(convertInternal(fn, inCall = true)) match {
            case Success(Some(callSite)) =>
              argumentsTree match {
                case Seq(s.OriginalTree(Some(range))) =>
                  // implicit conversion
                  extractedCallSites.updateValue(
                    callSite.callSiteId,
                    _.copy(
                      code = callSite.code + findTerm(range, ast).map(x => s"($x)").getOrElse("")
                    )
                  )
                case _ =>
                  val arguments =
                    argumentsTree
                      .zip(callSitesFromArguments)
                      .map(x => createArgument(callSite, x._1, x._2))
                      .collect { case Some(x) => x }
                      .toList

                  if (arguments.nonEmpty) {
                    extractedCallSiteArgumentsLists.updateValue(callSite.callSiteId, arguments :: _)
                  }
              }

              Some(callSite)
            case Success(None) =>
              throw UnExtractableCallSiteException(synthetic, db.uri)
            case Failure(cause) =>
              throw UnExtractableCallSiteException(synthetic, db.uri, cause)
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
                val location = Location(path, relativeUri, Some(range.toPos))
                Some(extractedCallSites.updateValue(cs.callSiteId, _.copy(location = location)))
              case _ =>
                convertInternal(qualifier, inCall = false)
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
          val term = findTerm(range, ast).getOrThrow {
            val text = scala.meta.inputs.Position
              .Range(
                ast.pos.input,
                range.startLine,
                range.startCharacter,
                range.endLine,
                range.endCharacter)
              .text
            SymbolNotFoundException(s"No term at ${db.uri}:$range ($text)")
          }

          val fnTerm = findFunctionTerm(term).getOrThrow {
            SymbolNotFoundException(s"No function term found for `$term` -- ${term.structure}")
          }

          // check if there is some inferred method call e.g.
          // Future(1) ==> Future.apply(1)
          // we want ot get a reference the actual method (apply), not object (Future)
          val cs = db.synthetics
            .find(term != fnTerm && _.range.exists(_ == fnTerm.pos.toRange))
            .flatMap(x => convertInternal(x.tree, inCall = true))
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
                val e = FunctionNotFoundException(term.syntax, range, db.uri)
                e
              }

              val declaration = ctx.resolveDeclaration(symbol)
              val location = Location(path, relativeUri, Some(range.toPos))
              val code = declaration.name

              createCallSite(declaration, code, location)
            }

          Some(cs)
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

      convertInternal(synthetic.tree, inCall = false)

      extractedCallSites.values.map { cs =>
        val hasImplicitParams = cs.declaration.hasImplicitParameters
        val recordedArgsLists = extractedCallSiteArgumentsLists.get(cs.callSiteId)

        val implicitArgumentsTypes =
          (hasImplicitParams, recordedArgsLists) match {
            case (true, Some(argsLists)) =>
              argsLists.head
            case (false, Some(_)) =>
              // this is OK since we record all argument lists
              // these are just a regular arguments
              Seq.empty
            case (false, None) =>
              Seq.empty
            case (true, None) =>
              throw ImplicitArgumentNotFoundException(cs.code, cs.declarationId)
          }

        cs.copy(implicitArgumentTypes = implicitArgumentsTypes)
      }
    }
  }

  def extractImplicitCallSites(
      moduleId: String,
      db: s.TextDocument,
      ast: Source): Iterable[Try[CallSite]] = {

    val converter = new Converter(moduleId, db, ast)
    implicit val declarationResolver: DeclarationResolver =
      ref => ctx.resolveDeclaration(ref.declarationId)(db)

    val result = db.synthetics.toList
      .map(x => x -> Try(converter.processSynthetic(x)))
      .collect {
        case (_, Success(css)) =>
          // next to call sites to implicit declarations, we also need to keep
          // call sites that are nested in there such as is the case in #22
          // essentially call sites created by macros
          css.filter(cs => cs.isImplicit || cs.parentId.isDefined).map(Success(_))
        case (synthetic, Failure(x)) =>
          List(Failure(new CallSiteConversionException(x, db.uri, synthetic)))
      }
      .flatten

    result
  }

  def callSiteCount(ast: Source)(implicit db: s.TextDocument): Int = {
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
        case Term.Assign(Term.Select(qual, _), rhs) =>
          n + 1 + process(0)(qual) + process(0)(rhs)
        case Term.Select(qual, _) =>
          n + 1 + process(0)(qual)

        case Term.ApplyInfix(lhs, _, _, args) =>
          n + 1 + process(0)(lhs) + args.map(process(0)).sum

        case Term.ApplyUnary(_, arg) =>
          n + 1 + process(0)(arg)

        case Term.Interpolate(_, _, args) =>
          n + 1 + args.map(process(0)).sum

        case Term.Block(stats) =>
          n + stats.map {
            // this is a call inside a block like
            // test {
            //   f
            // }
            // where f is a function
            case x: Term.Name =>
              Try(ctx.resolveSymbol(x.pos.toRange))
                .filter(_.symbolInfo.isMethod)
                .map(_ => 1)
                .getOrElse(0)
            case x =>
              process(0)(x)
          }.sum

        case Term.New(Init(_, _, argss)) =>
          n + 1 + argss.flatMap(x => x.map(process(0))).sum
        case Term.NewAnonymous(templ) =>
          n + 1 + process(0)(templ)

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

  private def findFunctionTerm(t: Tree): Option[Tree] = t match {
    case Term.Select(_, _) => Some(t)
    case Term.Name(_) => Some(t)
    case Term.ApplyType(fun, _) => findFunctionTerm(fun)
    case Term.Apply(fun, _) => findFunctionTerm(fun)
    case Term.New(init) => findFunctionTerm(init)
    case Term.NewAnonymous(Template(_, init :: _, _, _)) => findFunctionTerm(init)
    case Term.ApplyUnary(op, _) => Some(op)
    case Term.ApplyInfix(_, op, _, _) => Some(op)
    case Term.Assign(lhs, _) => findFunctionTerm(lhs)
    case Term.Interpolate(prefix, _,  _) => Some(prefix)
    case Term.This(_) => Some(t)
    case Init(_, name, _) => Some(name)
    case x: Lit if x.parent.isDefined => findFunctionTerm(x.parent.get)
    case _ =>
      None
  }

  private def findTerm(range: s.Range, ast: Source): Option[Tree] = {
    val target = MetaPos.Range(
      ast.pos.input,
      range.startLine,
      range.startCharacter,
      range.endLine,
      range.endCharacter
    )

    def containsTarget(x: Tree) =
      x.pos.start <= target.start && x.pos.end >= target.end

    def find(tree: Tree, lastTerm: Option[Tree]): Option[Tree] = {
      val lt = tree match {
        case x: Term => Some(x)
        case Defn.Class(_,_,_,_,Template(_, init :: _, _, _)) => Some(init)
        case _ => lastTerm
      }

      if (containsTarget(tree)) {
        tree.children
          .find(containsTarget)
          .flatMap(x => find(x, lt))
          .orElse(lt)
      } else {
        None
      }
    }

    find(ast, None)
  }
}
