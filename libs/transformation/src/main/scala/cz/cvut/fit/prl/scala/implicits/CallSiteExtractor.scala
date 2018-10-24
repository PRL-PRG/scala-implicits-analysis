//package cz.cvut.fit.prl.scala.implicits
//
//import cz.cvut.fit.prl.scala.implicits.SemanticdbSymbolResolver.ResolvedSymbol
//import cz.cvut.fit.prl.scala.implicits.model.Declaration
//import cz.cvut.fit.prl.scala.implicits.{model => m}
//import m.CallSite.Kind.{
//  CONVERSION => ConversionCall,
//  NORMAL => NormalCall,
//  SYNTHETIC => SyntheticCall
//}
//
//import scala.collection.mutable
//import scala.language.implicitConversions
//import scala.meta.internal.{semanticdb => s}
//import scala.meta.internal.semanticdb.Scala._
//import scala.meta._
//import scala.util.{Failure, Success, Try}
//import scala.meta.internal.symtab._
//import cz.cvut.fit.prl.scala.implicits.utils._
//
//import scala.meta.internal.semanticdb.MethodSignature
//
//
//object CallSiteExtractor {}
//
//class CallSiteExtractor(val db: s.TextDocument, val symtab: SymbolTable) {
//
//  import CallSiteExtractor._
//
//  implicit val resolver = SemanticdbSymbolResolver(db, symtab)
//
//  val tree: Tree = db.text.parse[Source].get
//  val synthetics: Seq[s.Synthetic] = db.synthetics
//
//  // TODO: move a function and make immutable
//  val declarations = mutable.Map[String, m.Declaration]()
//
//  private val extraction: (Seq[m.CallSite], Seq[Throwable]) = {
//    val (callSites, errors) = extractExplicitCallSites(tree).split()
//    val (updatedCallSites, syntheticErrors) =
//      updateWithSynthetics(callSites).split()
//
//    (updatedCallSites, errors ++ syntheticErrors)
//  }
//
//  val callSites: Seq[m.CallSite] = extraction._1
//
//  val failures: Seq[Throwable] = extraction._2
//
//  def extractDeclarations(): Seq[Declaration] = {
//    db.symbols.foreach(extractDeclaration)
//    declarations.values.toSeq
//  }
//
//  def extractDeclaration(symbolInfo: s.SymbolInformation): Unit =
//    if (symbolInfo.isImplicit) {
//      symbolInfo match {
//        case x
//            if x.isClass || x.isMethod || x.isObject || x.isVal || x.isVar || x.isMacro =>
//          resolveDeclaration(symbolInfo)
//        case x if x.isParameter =>
//          resolveDeclaration(x.symbol.owner)
//        case _ =>
//        // ignore
//      }
//    }
//
//  def findCallSiteFunctionSymbol(t: Term): Term.Name = t match {
//    case Term.Select(_, name)   => name
//    case Term.ApplyType(fun, _) => findCallSiteFunctionSymbol(fun)
//    case Term.Apply(fun, _)     => findCallSiteFunctionSymbol(fun)
//    case x: Term.Name           => x
//    case _ =>
//      throw new Exception(s"${t.structure} is not supported function name term")
//  }
//
//  def extractExplicitCallSites(tree: Tree): Seq[Try[m.CallSite]] = {
//
//    //    def resolveType(t: scala.meta.Type): Type = t match {
//    //      case Type.Name(_) =>
//    //        TypeRef(resolver.resolveSymbol(t.pos))
//    //      case Type.Apply(tpe, args) =>
//    //        ParamTypeRef(resolveType(tpe), args map resolveType)
//    //      case _ =>
//    //        throw new Exception(s"Unsupported type tree: $t")
//    //    }
//    //
//    //    def createArgument(css: List[NormalCall])(arg: Term): Argument = {
//    //      // TODO: do we actually need this - should be the first one right?
//    //      def findCallsiteFor(t: Term): NormalCall =
//    //        css.find(_.tree.pos == t.pos).getOrThrow(new Exception(s"Unable to find callsite for $t"))
//    //
//    //      arg match {
//    //        case x@(_: Term.Apply
//    //                | _: Term.Select
//    //                | _: Term.ApplyType
//    //                | _: Term.ApplyInfix
//    //                | _: Term.ApplyUnary
//    //                | _: Term.New
//    //                | _: Term.NewAnonymous
//    //                | _: Term.Interpolate) =>
//    //          val cs = findCallsiteFor(x)
//    //          CallSiteRef(cs)
//    //
//    //        case _: Term.Name =>
//    //          ValueRef(resolver.resolveSymbol(arg.pos))
//    //
//    //        case Term.Tuple(args) =>
//    //          Tuple(args map createArgument(css))
//    //
//    //        case Lit(value) =>
//    //          Literal(value)
//    //
//    //        case _: Term.Placeholder =>
//    //          Placeholder()
//    //
//    //        case _ =>
//    //          throw new Exception(s"Unsupported argument term: ${arg.structure} (${arg.toString}) at ${arg.pos.toRange}")
//    //      }
//    //    }
//
//    // TODO: ++ is too much - too many call sites potentially - should be Nil in args = process(Nil)
//    def process(css: List[Try[m.CallSite]])(tree: Tree): List[Try[m.CallSite]] =
//      tree match {
//        case Term.Apply(fun, args) => {
//          Try {
//            val argsCss = args flatMap process(css)
//            //            val arguments = args map createArgument(argsCss collect { case Success(x: m.CallSite) => x })
//            //            val argumentList = ArgumentsList(arguments, true)
//
//            fun match {
//              case x: Term.Name =>
//                val funSymbol = resolver.resolveSymbol(x.pos)
//                val declaration = resolveDeclaration(funSymbol)
//                val cs = m.CallSite(
//                  declaration.ref,
//                  NormalCall,
//                  tree.toString(),
//                  fun.pos
//                )
//
//                Success(cs) :: (argsCss ++ css)
//              case x =>
//                val Success(y) :: ys = process(css)(x)
//                val cs = y
//                // TODO: update arguments and tree
//                //                val cs = y.copy(tree = tree, lhsRange = fun.pos, argss = y.argss :+ argumentList)
//
//                Success(cs) :: (ys ++ argsCss) //  ++ css
//            }
//          } match {
//            case Success(x) => x
//            case Failure(x) => Failure(x) :: css
//          }
//        }
//
//        case Term.ApplyType(fun, targs) => {
//          Try {
//            //val typeArgs = targs map resolveType
//
//            fun match {
//              case x: Term.Name =>
//                val funSymbol = resolver.resolveSymbol(x.pos)
//                val declaration = resolveDeclaration(funSymbol)
//                val cs = m.CallSite(
//                  declaration.ref,
//                  NormalCall,
//                  tree.toString(),
//                  fun.pos
//                ) //, typeArgs = typeArgs)
//
//                Success(cs) :: css
//              case x =>
//                val Success(y) :: ys = process(css)(x)
//                val cs = y.copy(code = tree.toString()) //, lhsRange = fun.pos)//, typeArgs = typeArgs)
//
//                Success(cs) :: ys //(ys ++ css)
//            }
//          } match {
//            case Success(x) => x
//            case Failure(x) => Failure(x) :: css
//          }
//        }
//
//        case Term.Select(qual, name) => {
//          Try {
//            val funSymbol = resolver.resolveSymbol(name.pos)
//            val declaration = resolveDeclaration(funSymbol)
//            val cs =
//              m.CallSite(declaration.ref, NormalCall, tree.toString(), qual.pos)
//
//            Success(cs) :: process(css)(qual)
//          } match {
//            case Success(x) => x
//            case Failure(x) => Failure(x) :: css
//          }
//        }
//
//        case Term.ApplyInfix(lhs, op: Term.Name, targs, args) => {
//          Try {
//
//            val lhsCss = process(css)(lhs)
//            val argsCss = args flatMap process(css)
//            //val arguments = args map createArgument(argsCss collect { case Success(x: m.CallSite) => x })
//            //val argumentList = ArgumentsList(arguments, true)
//            val pos = {
//              val lr = lhs.pos
//              val or = op.pos
//              m.Position(
//                lr.startLine,
//                lr.startCharacter,
//                or.endLine,
//                or.endCharacter
//              )
//            }
//            val funSymbol = resolver.resolveSymbol(op.pos)
//            val declaration = resolveDeclaration(funSymbol)
//            //val typeArgs = targs map resolveType
//            val cs =
//              m.CallSite(declaration.ref, NormalCall, tree.toString(), pos)
//            //val cs = m.CallSite(declaration, tree, r, Seq(argumentList), typeArgs)
//
//            Success(cs) :: (lhsCss ++ argsCss ++ css)
//          } match {
//            case Success(x) => x
//            case Failure(x) => Failure(x) :: css
//          }
//        }
//
//        case Term.ApplyUnary(op, arg) => {
//          Try {
//            val argCss = process(css)(arg)
//            //val argument = createArgument(argCss collect { case Success(x: m.CallSite) => x })(arg)
//            //val argumentList = ArgumentsList(Seq(argument), true)
//            val funSymbol = resolver.resolveSymbol(op.pos)
//            val declaration = resolveDeclaration(funSymbol)
//            val cs = m.CallSite(
//              declaration.ref,
//              NormalCall,
//              tree.toString(),
//              op.pos
//            ) //, Seq(argumentList))
//
//            Success(cs) :: (argCss ++ css)
//          } match {
//            case Success(x) => x
//            case Failure(x) => Failure(x) :: css
//          }
//        }
//
//        case Term.Interpolate(prefix, _, args) => {
//          Try {
//            val argsCss = args flatMap process(css)
//            //val arguments = args map createArgument(argsCss collect { case Success(x: m.CallSite) => x })
//            //val argumentsList = ArgumentsList(arguments, true)
//            val funSymbol = resolver.resolveSymbol(prefix.pos)
//            val declaration = resolveDeclaration(funSymbol)
//            val pos = {
//              val x = prefix.pos
//              m.Position(
//                startLine = x.startLine,
//                endLine = x.startLine,
//                startCol = x.startColumn,
//                endCol = x.startColumn
//              )
//            }
//            val cs = m.CallSite(
//              declaration.ref,
//              NormalCall,
//              tree.toString(),
//              pos
//            ) //, argss = Seq(argumentsList))
//
//            Success(cs) :: (argsCss ++ css)
//          } match {
//            case Success(x) => x
//            case Failure(x) => Failure(x) :: css
//          }
//        }
//
//        case Term.New(Init(tpe, name, argss)) => {
//          Try {
//            val argsCss = argss.flatMap(_.flatMap(process(css)))
//            //val argumentss = argss map (args => args map createArgument(argsCss collect { case Success(x: m.CallSite) => x }))
//            //val argumentsLists = argumentss map (ArgumentsList(_, true))
//
//            // TODO: check lhsRange
//            // TODO: return type
//
//            val funSymbol = resolver.resolveSymbol(name.pos)
//            val declaration = resolveDeclaration(funSymbol)
//            //            val typeArgs = resolveType(tpe) match {
//            //              case ParamTypeRef(_, args) => args
//            //              case _ => Seq()
//            //            }
//
//            val cs = m.CallSite(
//              declaration.ref,
//              NormalCall,
//              tree.toString(),
//              name.pos
//            ) //, argumentsLists, typeArgs)
//
//            Success(cs) :: (argsCss ++ css)
//          } match {
//            case Success(x) => x
//            case Failure(x) => Failure(x) :: css
//          }
//        }
//
//        // TODO New.Anonymous
//
//        case Pkg(_, stats) =>
//          stats.flatMap(process(css))
//        case Ctor.Primary(_, _, paramss) =>
//          paramss.flatten.flatMap(process(css))
//        case Ctor.Secondary(_, _, paramss, _, stats) =>
//          paramss.flatten.flatMap(process(css)) ++ stats.flatMap(process(css))
//        case Term.Param(_, _, _, Some(default)) =>
//          process(css)(default)
//        case Term.Param(_, _, _, None) =>
//          css
//        case Defn.Trait(_, _, _, ctor, templ) =>
//          process(css)(ctor) ++ process(css)(templ)
//        case Defn.Class(_, _, _, ctor, templ) =>
//          process(css)(ctor) ++ process(css)(templ)
//        case Defn.Object(_, _, templ) =>
//          process(css)(templ)
//        case Defn.Var(_, _, _, Some(rhs)) =>
//          process(css)(rhs)
//        case Defn.Var(_, _, _, None) =>
//          css
//        case Defn.Val(_, _, _, rhs) =>
//          process(css)(rhs)
//        case Defn.Def(_, _, _, paramss, _, body) =>
//          paramss.flatten.flatMap(process(css)) ++ process(css)(body)
//        case _ @(_: Defn.Type | _: Import) =>
//          css
//        case t =>
//          t.children flatMap process(css)
//      }
//
//    process(Nil)(tree)
//  }
//
//  // TODO: refactor
//  object idTreeMethod {
//    def unapply(
//      t: s.IdTree
//    )(implicit resolver: SymbolResolver): Option[s.SymbolInformation] = {
//      val symbol = resolver.resolveSymbol(t.symbol)
//      if (symbol.isMethod && (symbol.properties & (0x800 | 0x400)) == 0) {
//        Some(symbol)
//      } else {
//        None
//      }
//    }
//  }
//
//  // TODO: refactor
//  object idTreeValue {
//    def unapply(
//      t: s.IdTree
//    )(implicit resolver: SymbolResolver): Option[s.SymbolInformation] = {
//      val symbol = resolver.resolveSymbol(t.symbol)
//      if (symbol.isMethod && (symbol.properties & (0x800 | 0x400)) > 0) {
//        Some(symbol)
//      } else {
//        None
//      }
//    }
//  }
//
//  //  final def createArgument(t: s.Tree, cs: Option[m.CallSite]): Argument =
//  //    (cs, t) match {
//  //      case (Some(x), idTreeMethod(_) | _: s.TypeApplyTree | _: s.SelectTree | _: s.ApplyTree) =>
//  //        CallSiteRef(x)
//  //      case (None, idTreeValue(symbol)) =>
//  //        ValueRef(symbol)
//  //      case (None, s.OriginalTree(Some(range))) =>
//  //        CodeRef(range)
//  //      case (_, s.FunctionTree(params, term)) =>
//  //        Literal(t)
//  //      case _ =>
//  //        throw new Exception(s"Unsupported semanticdb argument $t")
//  //    }
//  //
//  //  final def resolveType(x: s.Type): Type = x match {
//  //    case s.TypeRef(_, symbol, Seq()) =>
//  //      TypeRef(resolver.resolveSymbol(symbol))
//  //    case s.TypeRef(_, symbol, targs) =>
//  //      ParamTypeRef(TypeRef(resolver.resolveSymbol(symbol)), targs map resolveType)
//  //    case s.Type.Empty =>
//  //      Empty
//  //    case _ =>
//  //      throw new Exception(s"Unsupported semanticdb type $x")
//  //  }
//
//  @scala.annotation.tailrec
//  final def symbolName(t: s.Tree): String = t match {
//    case s.IdTree(symbol)          => symbol
//    case s.SelectTree(_, Some(fn)) => symbolName(fn)
//    case s.TypeApplyTree(fn, _)    => symbolName(fn)
//    case x                         => throw new Exception(s"Unable to resolve $x")
//  }
//
//  def resolveName(t: s.Tree): s.SymbolInformation = {
//    resolver.resolveSymbol(symbolName(t))
//  }
//
//  def updateWithSynthetics(
//    explicitCallsites: Seq[m.CallSite]
//  ): Seq[Try[m.CallSite]] = {
//    val callSitesIndex = explicitCallsites.to[mutable.ArrayBuffer]
//    val errors = mutable.ArrayBuffer[Throwable]()
//
//    def process(node: s.Tree): Option[Int] = node match {
//      // implicit conversion
//      case s.ApplyTree(fn, Seq(from @ s.OriginalTree(Some(range)))) => {
//        val resolvedFun = resolveName(fn)
//        val declaration = resolveDeclaration(resolvedFun)
//        val cs =
//          m.CallSite(declaration.ref, ConversionCall, node.toString, range)
//        // val cs = m.CallSite(declaration, range, Seq(ArgumentsList(Seq(createArgument(from, None)), false)))
//        callSitesIndex += cs
//
//        process(fn)
//      }
//
//      // implicit arguments (e.g. `(Seq(1) ++ Seq(2))(canBuildFrom)`)
//      case s.ApplyTree(fun, implicitArgs) => {
//        val idx = fun match {
//          case s.OriginalTree(Some(range)) =>
//            val idx = callSitesIndex.lastIndexWhere {
//              case m.CallSite(_, _, _, pos: m.Position, _) =>
//                pos == range.toLocal
//              case _ => false
//            }
//            if (idx == -1) {
//              throw new Exception(
//                s"Unable to find call site for ApplyTree $fun in $node"
//              )
//            }
//
//            idx
//          case _ =>
//            process(fun).getOrThrow(
//              new Exception(s"Unable to find call site for ApplyTree $node")
//            )
//        }
//
//        val argsCss = implicitArgs map process
//        //val resolvedArgs = (implicitArgs zip (argsCss map (_ map callSitesIndex))) map (x => createArgument(x._1, x._2))
//        //val args = ArgumentsList(resolvedArgs, false)
//        val cs = callSitesIndex(idx)
//        // TODO: update args
//        //        callSitesIndex.update(
//        //          idx,
//        //          cs.update(argss = cs.argss :+ args)
//        //        )
//
//        Some(idx)
//      }
//
//      // inferred type arguments (e.g. `Seq(1)` -> `Seq[Int](1)`)
//      case s.TypeApplyTree(fun, targs) => {
//        val idx = fun match {
//          case s.OriginalTree(Some(range)) =>
//            val idx = callSitesIndex.lastIndexWhere {
//              case m.CallSite(_, _, _, pos: m.Position, _) =>
//                pos == range.toLocal
//              case _ => false
//            }
//            if (idx == -1) {
//              throw new Exception(
//                s"Unable to find call site for TypeApplyTree $fun in $node"
//              )
//            }
//            idx
//
//          case _ =>
//            process(fun).getOrThrow(
//              new Exception(s"Unable to find call site for TypeApplyTree $node")
//            )
//        }
//
//        // TODO: update type argumemnts
//        //        val typeArgs = targs map resolveType
//        //        val cs = callSitesIndex(idx)
//        //        callSitesIndex.update(
//        //          idx,
//        //          cs.update(typeArgs = typeArgs)
//        //        )
//
//        Some(idx)
//      }
//
//      // inferred method calls
//      case s.SelectTree(qual, Some(fn)) => {
//        val resolvedFun = resolveName(fn)
//        val declaration = resolveDeclaration(resolvedFun)
//        val idx = qual match {
//          case s.OriginalTree(Some(range)) =>
//            val idx =
//              // case for .apply .unapply, ...
//              callSitesIndex.lastIndexWhere {
//                case m.CallSite(_, _, _, pos, _) => pos == range.toLocal
//                case _                           => false
//              } match {
//                case -1 =>
//                  // case for .map, .flatMap, ...
//                  // we need to create a synthetic call
//                  val cs = m.CallSite(
//                    declaration.ref,
//                    SyntheticCall,
//                    tree.toString(),
//                    tree.pos
//                  )
//                  callSitesIndex += cs
//                  callSitesIndex.size - 1
//                case x => x
//              }
//            if (idx == -1) {
//              throw new Exception(
//                s"Unable to find call site for SelectTree $qual in $node"
//              )
//            }
//            idx
//
//          case _ =>
//            process(qual).getOrThrow(
//              new Exception(s"Unable to find call site for SelectTree $node")
//            )
//        }
//
//        val cs = callSitesIndex(idx)
//
//        callSitesIndex.update(idx, cs.copy(declarationRef = declaration.ref))
//
//        Some(idx)
//      }
//
//      case idTreeMethod(symbol) => {
//        val declaration = resolveDeclaration(symbol)
//        val cs =
//          m.CallSite(declaration.ref, SyntheticCall, tree.toString(), tree.pos)
//
//        callSitesIndex += cs
//
//        Some(callSitesIndex.size - 1)
//      }
//
//      case s.IdTree(_) =>
//        // do nothing
//        None
//
//      case s.FunctionTree(params, term) =>
//        ((params :+ term) flatMap process).headOption
//
//      case node @ s.MacroExpansionTree(expandee, _) =>
//        process(expandee)
//
//      case s.LiteralTree(_) =>
//        // do nothing
//        None
//
//      case s.OriginalTree(_) =>
//        // do nothing
//        None
//
//      case x =>
//        throw new Exception(s"Unexpected synthetic tree $x")
//    }
//
//    val run = synthetics map { x =>
//      try {
//        process(x.tree)
//      } catch {
//        case e: Throwable => errors += e
//      }
//    }
//
//    val good = callSitesIndex map Success.apply
//    val bad = errors map Failure.apply
//
//    good ++ bad
//  }
//}
