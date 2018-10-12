package cz.cvut.fit.prl.scala.implicits

import cz.cvut.fit.prl.scala.implicits.{model => m}
import m.CallSite.Kind.{NORMAL => NormalCall, SYNTHETIC => SyntheticCall, CONVERSION => ConversionCall}

import scala.collection.mutable
import scala.language.implicitConversions
import scala.meta.internal.{semanticdb => s}
import scala.meta.internal.semanticdb.Scala._
import scala.meta._
import scala.util.{Failure, Success, Try}
import scala.meta.internal.symtab._
import cz.cvut.fit.prl.scala.implicits.utils._


trait SymbolResolver {
  def resolveSymbol(range: s.Range): s.SymbolInformation

  def resolveSymbol(name: String): s.SymbolInformation
}

case class SemanticdbSymbolResolver(db: s.TextDocument, symtab: SymbolTable) extends SymbolResolver {

  case class ResolvedSymbol(occurence: s.SymbolOccurrence) {
    lazy val info: Option[s.SymbolInformation] =
      symtab.info(occurence.symbol).orElse(db.symbols.find(_.symbol == occurence.symbol))
  }

  private val symbols: Map[s.Range, ResolvedSymbol] = db.occurrences.collect {
    case s@s.SymbolOccurrence(Some(range), _, _) => range -> ResolvedSymbol(s)
  }.toMap

  def resolveSymbol(range: s.Range): s.SymbolInformation = {
    val resolvedSymbol = symbols(range)
    resolvedSymbol
      .info
      .getOrThrow(new Exception(s"Symbol at range $range (${resolvedSymbol.occurence.symbol}) is not in semanticdb occurencies"))
  }

  def resolveSymbol(name: String): s.SymbolInformation =
    db.symbols.find(_.symbol == name)
      .orElse(symtab.info(name))
      .getOrThrow(new Exception(s"Unable to find $name in symtab or in local symbols"))
}


object CallSiteExtractor {

}

class CallSiteExtractor(val db: s.TextDocument, val symtab: SymbolTable) {

  import CallSiteExtractor._

  implicit val resolver = SemanticdbSymbolResolver(db, symtab)

  val tree: Tree = db.text.parse[Source].get
  val synthetics: Seq[s.Synthetic] = db.synthetics

  // TODO: move a function and make immutable
  val declarations = mutable.Map[String, m.Declaration]()

  private val extraction: (Seq[m.CallSite], Seq[Throwable]) = {
    val (callSites, errors) = extractExplicitCallSites(tree).split()
    val (updatedCallSites, syntheticErrors) = updateWithSynthetics(callSites).split()

    (updatedCallSites, errors ++ syntheticErrors)
  }

  val callSites: Seq[m.CallSite] = extraction._1

  val failures: Seq[Throwable] = extraction._2

  def extractDeclarations(): Unit = {
    declarations ++=
      db.symbols
        .filter(x => x.isImplicit && (x.isClass || x.isMethod))
        .map(x => x.symbol -> createDeclaration(x))

    // TODO: all implicit parameters
  }

  def createSymbol(symbolInfo: s.SymbolInformation): m.Symbol = {
    val location = m.Location("", false)
    m.Symbol(symbolInfo.symbol, symbolInfo.displayName, location, symbolInfo.language, symbolInfo.isImplicit)
  }

  def createDeclaration(symbolInfo: s.SymbolInformation): m.Declaration = {
    val symbol = createSymbol(symbolInfo)
    // TODO: objects
    // TODO: variables/values - they are represented as methods
    // TODO: macros
    // TODO: process annotations
    symbolInfo match {
      case tpe: s.SymbolInformation if tpe.isType || tpe.isClass || tpe.isTrait || tpe.isInterface =>
        m.TypeDeclaration(symbol = symbol)

      case mtd: s.SymbolInformation if mtd.isMethod =>
        m.MethodDeclaration(
          symbol = symbol,
          returnType = null
        )
      case _ =>
        throw new Exception(s"SDB SymbolInfo `$symbolInfo` is not support yet")
    }
  }

  def updateWithSignature(declaration: m.Declaration, signature: s.Signature): m.Declaration =
    (declaration, signature) match {
      case (model: m.TypeDeclaration, sdb: s.TypeSignature) =>
        val typeParameters = sdb.typeParameters.symbols.map(createTypeParameter)

        model.copy(typeParameters = typeParameters)

      case (model: m.TypeDeclaration, sdb: s.ClassSignature) =>
        val typeParameters = sdb.typeParameters.symbols.map(createTypeParameter)
        val parents = sdb.parents.map(createType)

        model.copy(typeParameters = typeParameters, parents = parents)

      case (model: m.MethodDeclaration, sdb: s.MethodSignature) =>
        val typeParameters = sdb.typeParameters.symbols.map(createTypeParameter)
        val parameterLists = sdb.parameterLists.map(createParameterList)
        val returnType = createType(sdb.returnType)

        model.copy(
          typeParameters = typeParameters,
          parameterLists = parameterLists,
          returnType = returnType
        )

      case _ => declaration
    }

  def createParameterList(scope: s.Scope): m.ParameterList = {
    val parameters = scope.symbols.map(resolver.resolveSymbol).map(createParameter)
    m.ParameterList(parameters)
  }

  def createParameter(symbolInfo: s.SymbolInformation): m.Parameter = symbolInfo.signature match {
    case s.ValueSignature(tpe) =>
      m.Parameter(symbolInfo.displayName, createType(tpe), symbolInfo.isImplicit)
    case x =>
      throw new Exception(s"Unexpected signature `$x`")
  }

  def createTypeParameter(symbol: String): m.TypeParameter = createTypeParameter(resolver.resolveSymbol(symbol))
  def createTypeParameter(symbolInfo: s.SymbolInformation): m.TypeParameter = symbolInfo.signature match {
    case s.TypeSignature(typeParameters, lowerBound, upperBound) =>
      m.TypeParameter(
        name=symbolInfo.displayName,
        typeParameters=typeParameters.symbols.map(createTypeParameter),
        lowerBound=createType(lowerBound),
        upperBound=createType(upperBound)
      )
    case x =>
      throw new Exception(s"Unexpected signature `$x`")
  }

  def createType(tpe: s.Type): m.Type = tpe match {
      // TODO: do we need to do anything prefix?
    case s.TypeRef(_, symbol, typeArguments) =>
      resolver.resolveSymbol(symbol) match {
        case x if x.isTypeParameter =>
          val parent = resolveDeclaration(symbol.owner)
          m.TypeParameterRef(parent.ref, x.displayName, typeArguments.map(createType))
        case x if x.isType || x.isClass || x.isTrait || x.isInterface =>
          val parent = resolveDeclaration(symbol)
          m.TypeRef(parent.ref, typeArguments.map(createType))
        case x =>
          throw new Exception(s"SDB Type `$x` is not supported yet")
      }
    case s.AnnotatedType(annotations, t) =>
      // TODO: annotation
      createType(t)
    case _ =>
      throw new Exception(s"SDB Type `$tpe` is not supported yet")
  }

  def resolveDeclaration(symbol: String): m.Declaration =
    resolveDeclaration(resolver.resolveSymbol(symbol))

  def resolveDeclaration(symbolInfo: s.SymbolInformation): m.Declaration = {
    declarations.get(symbolInfo.symbol) match {
      case Some(decl) => decl
      case None =>
        // Since some declarations can contain cycles (e.g. class X[A <: X[A]])
        // we have to build it in two phases: first create a prototype and then
        // resolve the remaining fields. It should be safe since we only use
        // references which are fixed from prototypes
        val prototype = createDeclaration(symbolInfo)
        declarations += symbolInfo.symbol -> prototype
        val declaration = updateWithSignature(prototype, symbolInfo.signature)
        declarations += symbolInfo.symbol -> declaration
        declaration
    }
  }

  def findCallSiteFunctionSymbol(t: Term): Term.Name = t match {
    case Term.Select(_, name) => name
    case Term.ApplyType(fun, _) => findCallSiteFunctionSymbol(fun)
    case Term.Apply(fun, _) => findCallSiteFunctionSymbol(fun)
    case x: Term.Name => x
    case _ => throw new Exception(s"${t.structure} is not supported function name term")
  }

  def extractExplicitCallSites(tree: Tree): Seq[Try[m.CallSite]] = {

    //    def resolveType(t: scala.meta.Type): Type = t match {
    //      case Type.Name(_) =>
    //        TypeRef(resolver.resolveSymbol(t.pos))
    //      case Type.Apply(tpe, args) =>
    //        ParamTypeRef(resolveType(tpe), args map resolveType)
    //      case _ =>
    //        throw new Exception(s"Unsupported type tree: $t")
    //    }
    //
    //    def createArgument(css: List[NormalCall])(arg: Term): Argument = {
    //      // TODO: do we actually need this - should be the first one right?
    //      def findCallsiteFor(t: Term): NormalCall =
    //        css.find(_.tree.pos == t.pos).getOrThrow(new Exception(s"Unable to find callsite for $t"))
    //
    //      arg match {
    //        case x@(_: Term.Apply
    //                | _: Term.Select
    //                | _: Term.ApplyType
    //                | _: Term.ApplyInfix
    //                | _: Term.ApplyUnary
    //                | _: Term.New
    //                | _: Term.NewAnonymous
    //                | _: Term.Interpolate) =>
    //          val cs = findCallsiteFor(x)
    //          CallSiteRef(cs)
    //
    //        case _: Term.Name =>
    //          ValueRef(resolver.resolveSymbol(arg.pos))
    //
    //        case Term.Tuple(args) =>
    //          Tuple(args map createArgument(css))
    //
    //        case Lit(value) =>
    //          Literal(value)
    //
    //        case _: Term.Placeholder =>
    //          Placeholder()
    //
    //        case _ =>
    //          throw new Exception(s"Unsupported argument term: ${arg.structure} (${arg.toString}) at ${arg.pos.toRange}")
    //      }
    //    }

    // TODO: ++ is too much - too many call sites potentially - should be Nil in args = process(Nil)
    def process(css: List[Try[m.CallSite]])(tree: Tree): List[Try[m.CallSite]] =
      tree match {
        case Term.Apply(fun, args) => {
          Try {
            val argsCss = args flatMap process(css)
            //            val arguments = args map createArgument(argsCss collect { case Success(x: m.CallSite) => x })
            //            val argumentList = ArgumentsList(arguments, true)

            fun match {
              case x: Term.Name =>
                val funSymbol = resolver.resolveSymbol(x.pos)
                val declaration = resolveDeclaration(funSymbol)
                val cs = m.CallSite(declaration.ref, NormalCall, tree.toString(), fun.pos)

                Success(cs) :: (argsCss ++ css)
              case x =>
                val Success(y) :: ys = process(css)(x)
                val cs = y
                // TODO: update arguments and tree
                //                val cs = y.copy(tree = tree, lhsRange = fun.pos, argss = y.argss :+ argumentList)

                Success(cs) :: (ys ++ argsCss) //  ++ css
            }
          } match {
            case Success(x) => x
            case Failure(x) => Failure(x) :: css
          }
        }

        case Term.ApplyType(fun, targs) => {
          Try {
            //val typeArgs = targs map resolveType

            fun match {
              case x: Term.Name =>
                val funSymbol = resolver.resolveSymbol(x.pos)
                val declaration = resolveDeclaration(funSymbol)
                val cs = m.CallSite(declaration.ref, NormalCall, tree.toString(), fun.pos) //, typeArgs = typeArgs)

                Success(cs) :: css
              case x =>
                val Success(y) :: ys = process(css)(x)
                val cs = y.copy(code = tree.toString()) //, lhsRange = fun.pos)//, typeArgs = typeArgs)

                Success(cs) :: ys //(ys ++ css)
            }
          } match {
            case Success(x) => x
            case Failure(x) => Failure(x) :: css
          }
        }

        case Term.Select(qual, name) => {
          Try {
            val funSymbol = resolver.resolveSymbol(name.pos)
            val declaration = resolveDeclaration(funSymbol)
            val cs = m.CallSite(declaration.ref, NormalCall, tree.toString(), qual.pos)

            Success(cs) :: process(css)(qual)
          } match {
            case Success(x) => x
            case Failure(x) => Failure(x) :: css
          }
        }

        case Term.ApplyInfix(lhs, op: Term.Name, targs, args) => {
          Try {

            val lhsCss = process(css)(lhs)
            val argsCss = args flatMap process(css)
            //val arguments = args map createArgument(argsCss collect { case Success(x: m.CallSite) => x })
            //val argumentList = ArgumentsList(arguments, true)
            val pos = {
              val lr = lhs.pos
              val or = op.pos
              m.Position(lr.startLine, lr.startCharacter, or.endLine, or.endCharacter)
            }
            val funSymbol = resolver.resolveSymbol(op.pos)
            val declaration = resolveDeclaration(funSymbol)
            //val typeArgs = targs map resolveType
            val cs = m.CallSite(declaration.ref, NormalCall, tree.toString(), pos)
            //val cs = m.CallSite(declaration, tree, r, Seq(argumentList), typeArgs)

            Success(cs) :: (lhsCss ++ argsCss ++ css)
          } match {
            case Success(x) => x
            case Failure(x) => Failure(x) :: css
          }
        }

        case Term.ApplyUnary(op, arg) => {
          Try {
            val argCss = process(css)(arg)
            //val argument = createArgument(argCss collect { case Success(x: m.CallSite) => x })(arg)
            //val argumentList = ArgumentsList(Seq(argument), true)
            val funSymbol = resolver.resolveSymbol(op.pos)
            val declaration = resolveDeclaration(funSymbol)
            val cs = m.CallSite(declaration.ref, NormalCall, tree.toString(), op.pos) //, Seq(argumentList))

            Success(cs) :: (argCss ++ css)
          } match {
            case Success(x) => x
            case Failure(x) => Failure(x) :: css
          }
        }

        case Term.Interpolate(prefix, _, args) => {
          Try {
            val argsCss = args flatMap process(css)
            //val arguments = args map createArgument(argsCss collect { case Success(x: m.CallSite) => x })
            //val argumentsList = ArgumentsList(arguments, true)
            val funSymbol = resolver.resolveSymbol(prefix.pos)
            val declaration = resolveDeclaration(funSymbol)
            val pos = {
              val x = prefix.pos
              m.Position(startLine = x.startLine, endLine = x.startLine, startCol = x.startColumn, endCol = x.startColumn)
            }
            val cs = m.CallSite(declaration.ref, NormalCall, tree.toString(), pos) //, argss = Seq(argumentsList))

            Success(cs) :: (argsCss ++ css)
          } match {
            case Success(x) => x
            case Failure(x) => Failure(x) :: css
          }
        }

        case Term.New(Init(tpe, name, argss)) => {
          Try {
            val argsCss = argss.flatMap(_.flatMap(process(css)))
            //val argumentss = argss map (args => args map createArgument(argsCss collect { case Success(x: m.CallSite) => x }))
            //val argumentsLists = argumentss map (ArgumentsList(_, true))

            // TODO: check lhsRange
            // TODO: return type

            val funSymbol = resolver.resolveSymbol(name.pos)
            val declaration = resolveDeclaration(funSymbol)
            //            val typeArgs = resolveType(tpe) match {
            //              case ParamTypeRef(_, args) => args
            //              case _ => Seq()
            //            }

            val cs = m.CallSite(declaration.ref, NormalCall, tree.toString(), name.pos) //, argumentsLists, typeArgs)

            Success(cs) :: (argsCss ++ css)
          } match {
            case Success(x) => x
            case Failure(x) => Failure(x) :: css
          }
        }

        // TODO New.Anonymous

        case Pkg(_, stats) =>
          stats.flatMap(process(css))
        case Ctor.Primary(_, _, paramss) =>
          paramss.flatten.flatMap(process(css))
        case Ctor.Secondary(_, _, paramss, _, stats) =>
          paramss.flatten.flatMap(process(css)) ++ stats.flatMap(process(css))
        case Term.Param(_, _, _, Some(default)) =>
          process(css)(default)
        case Term.Param(_, _, _, None) =>
          css
        case Defn.Trait(_, _, _, ctor, templ) =>
          process(css)(ctor) ++ process(css)(templ)
        case Defn.Class(_, _, _, ctor, templ) =>
          process(css)(ctor) ++ process(css)(templ)
        case Defn.Object(_, _, templ) =>
          process(css)(templ)
        case Defn.Var(_, _, _, Some(rhs)) =>
          process(css)(rhs)
        case Defn.Var(_, _, _, None) =>
          css
        case Defn.Val(_, _, _, rhs) =>
          process(css)(rhs)
        case Defn.Def(_, _, _, paramss, _, body) =>
          paramss.flatten.flatMap(process(css)) ++ process(css)(body)
        case _@(_: Defn.Type | _: Import) =>
          css
        case t =>
          t.children flatMap process(css)
      }

    process(Nil)(tree)
  }

  // TODO: refactor
  object idTreeMethod {
    def unapply(t: s.IdTree)(implicit resolver: SymbolResolver): Option[s.SymbolInformation] = {
      val symbol = resolver.resolveSymbol(t.symbol)
      if (symbol.isMethod && (symbol.properties & (0x800 | 0x400)) == 0) {
        Some(symbol)
      } else {
        None
      }
    }
  }

  // TODO: refactor
  object idTreeValue {
    def unapply(t: s.IdTree)(implicit resolver: SymbolResolver): Option[s.SymbolInformation] = {
      val symbol = resolver.resolveSymbol(t.symbol)
      if (symbol.isMethod && (symbol.properties & (0x800 | 0x400)) > 0) {
        Some(symbol)
      } else {
        None
      }
    }
  }

  //  final def createArgument(t: s.Tree, cs: Option[m.CallSite]): Argument =
  //    (cs, t) match {
  //      case (Some(x), idTreeMethod(_) | _: s.TypeApplyTree | _: s.SelectTree | _: s.ApplyTree) =>
  //        CallSiteRef(x)
  //      case (None, idTreeValue(symbol)) =>
  //        ValueRef(symbol)
  //      case (None, s.OriginalTree(Some(range))) =>
  //        CodeRef(range)
  //      case (_, s.FunctionTree(params, term)) =>
  //        Literal(t)
  //      case _ =>
  //        throw new Exception(s"Unsupported semanticdb argument $t")
  //    }
  //
  //  final def resolveType(x: s.Type): Type = x match {
  //    case s.TypeRef(_, symbol, Seq()) =>
  //      TypeRef(resolver.resolveSymbol(symbol))
  //    case s.TypeRef(_, symbol, targs) =>
  //      ParamTypeRef(TypeRef(resolver.resolveSymbol(symbol)), targs map resolveType)
  //    case s.Type.Empty =>
  //      Empty
  //    case _ =>
  //      throw new Exception(s"Unsupported semanticdb type $x")
  //  }

  @scala.annotation.tailrec
  final def symbolName(t: s.Tree): String = t match {
    case s.IdTree(symbol) => symbol
    case s.SelectTree(_, Some(fn)) => symbolName(fn)
    case s.TypeApplyTree(fn, _) => symbolName(fn)
    case x => throw new Exception(s"Unable to resolve $x")
  }

  def resolveName(t: s.Tree): s.SymbolInformation = {
    resolver.resolveSymbol(symbolName(t))
  }

  def updateWithSynthetics(explicitCallsites: Seq[m.CallSite]): Seq[Try[m.CallSite]] = {
    val callSitesIndex = explicitCallsites.to[mutable.ArrayBuffer]
    val errors = mutable.ArrayBuffer[Throwable]()

    def process(node: s.Tree): Option[Int] = node match {
      // implicit conversion
      case s.ApplyTree(fn, Seq(from@s.OriginalTree(Some(range)))) => {
        val resolvedFun = resolveName(fn)
        val declaration = resolveDeclaration(resolvedFun)
        val cs = m.CallSite(declaration.ref, ConversionCall, node.toString, range)
        // val cs = m.CallSite(declaration, range, Seq(ArgumentsList(Seq(createArgument(from, None)), false)))
        callSitesIndex += cs

        process(fn)
      }

      // implicit arguments (e.g. `(Seq(1) ++ Seq(2))(canBuildFrom)`)
      case s.ApplyTree(fun, implicitArgs) => {
        val idx = fun match {
          case s.OriginalTree(Some(range)) =>
            val idx = callSitesIndex.lastIndexWhere {
              case m.CallSite(_, _, _, pos: m.Position, _) => pos == range.toLocal
              case _ => false
            }
            if (idx == -1) {
              throw new Exception(s"Unable to find call site for ApplyTree $fun in $node")
            }

            idx
          case _ =>
            process(fun).getOrThrow(new Exception(s"Unable to find call site for ApplyTree $node"))
        }

        val argsCss = implicitArgs map process
        //val resolvedArgs = (implicitArgs zip (argsCss map (_ map callSitesIndex))) map (x => createArgument(x._1, x._2))
        //val args = ArgumentsList(resolvedArgs, false)
        val cs = callSitesIndex(idx)
        // TODO: update args
        //        callSitesIndex.update(
        //          idx,
        //          cs.update(argss = cs.argss :+ args)
        //        )

        Some(idx)
      }

      // inferred type arguments (e.g. `Seq(1)` -> `Seq[Int](1)`)
      case s.TypeApplyTree(fun, targs) => {
        val idx = fun match {
          case s.OriginalTree(Some(range)) =>
            val idx = callSitesIndex.lastIndexWhere {
              case m.CallSite(_, _, _, pos: m.Position, _) => pos == range.toLocal
              case _ => false
            }
            if (idx == -1) {
              throw new Exception(s"Unable to find call site for TypeApplyTree $fun in $node")
            }
            idx

          case _ =>
            process(fun).getOrThrow(new Exception(s"Unable to find call site for TypeApplyTree $node"))
        }

        // TODO: update type argumemnts
        //        val typeArgs = targs map resolveType
        //        val cs = callSitesIndex(idx)
        //        callSitesIndex.update(
        //          idx,
        //          cs.update(typeArgs = typeArgs)
        //        )

        Some(idx)
      }

      // inferred method calls
      case s.SelectTree(qual, Some(fn)) => {
        val resolvedFun = resolveName(fn)
        val declaration = resolveDeclaration(resolvedFun)
        val idx = qual match {
          case s.OriginalTree(Some(range)) =>
            val idx =
            // case for .apply .unapply, ...
              callSitesIndex.lastIndexWhere {
                case m.CallSite(_, _, _, pos, _) => pos == range.toLocal
                case _ => false
              } match {
                case -1 =>
                  // case for .map, .flatMap, ...
                  // we need to create a synthetic call
                  val cs = m.CallSite(declaration.ref, SyntheticCall, tree.toString(), tree.pos)
                  callSitesIndex += cs
                  callSitesIndex.size - 1
                case x => x
              }
            if (idx == -1) {
              throw new Exception(s"Unable to find call site for SelectTree $qual in $node")
            }
            idx

          case _ =>
            process(qual).getOrThrow(new Exception(s"Unable to find call site for SelectTree $node"))
        }

        val cs = callSitesIndex(idx)

        callSitesIndex.update(
          idx,
          cs.copy(declarationRef = declaration.ref)
        )

        Some(idx)
      }

      case idTreeMethod(symbol) => {
        val declaration = resolveDeclaration(symbol)
        val cs = m.CallSite(declaration.ref, SyntheticCall, tree.toString(), tree.pos)

        callSitesIndex += cs

        Some(callSitesIndex.size - 1)
      }

      case s.IdTree(_) =>
        // do nothing
        None

      case s.FunctionTree(params, term) =>
        ((params :+ term) flatMap process).headOption

      case node@s.MacroExpansionTree(expandee, _) =>
        process(expandee)

      case s.LiteralTree(_) =>
        // do nothing
        None

      case s.OriginalTree(_) =>
        // do nothing
        None

      case x =>
        throw new Exception(s"Unexpected synthetic tree $x")
    }

    val run = synthetics map { x =>
      try {
        process(x.tree)
      } catch {
        case e: Throwable => errors += e
      }
    }

    val good = callSitesIndex map Success.apply
    val bad = errors map Failure.apply

    good ++ bad
  }
}
