package cz.cvut.fit.prl.scala.implicits

import scala.meta.internal.semanticdb.SymbolInformation.Property.IMPLICIT
import scala.meta.internal.semanticdb.SymbolInformation.Kind
import scala.collection.mutable
import scala.language.implicitConversions
import scala.meta.internal.{semanticdb => s}
import scala.meta._
import scala.util.{Failure, Success, Try}
import scala.meta.internal.symtab._
import cz.cvut.fit.prl.scala.implicits.utils._

import scala.collection.mutable.ArrayBuffer
import scala.tools.jline_embedded.console.completer.ArgumentCompleter.ArgumentList

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

case class Declaration(
                        kind: Kind,
                        fqn: String,
                        name: String,
                        isImplicit: Boolean,
                        parameterLists: Seq[ParameterList]
                      ) {
  def returnType: Type = ???

}

object Declaration {
  implicit def apply(x: s.SymbolInformation)(implicit context: SymbolResolver): Declaration = {
    val parameterLists: Seq[ParameterList] = x.signature match {
      case x: s.MethodSignature =>
        x.parameterLists.map(ParameterList(_))
      case _ => Seq()
    }

    Declaration(x.kind, x.symbol, x.displayName, (x.properties & IMPLICIT.value) > 0, parameterLists)
  }
}

case class ParameterList(isImplicit: Boolean)

object ParameterList {
  implicit def apply(x: s.Scope)(implicit context: SymbolResolver): ParameterList = {
    val isImplicit =
      x.symlinks
        .headOption
        .exists(x => (context.resolveSymbol(x).properties & IMPLICIT.value) > 0)

    ParameterList(isImplicit)
  }
}

sealed trait Type {
  def code: String
}

case object Empty extends Type {
  val code: String = "EMPTY"
}

case class TypeRef(symbol: s.SymbolInformation) extends Type {
  val code: String = symbol.displayName
}

case class ParamTypeRef(ref: Type, args: Seq[Type]) extends Type {
  val code: String = s"$ref${args.map(_.code).mkStringOpt("[", ",", "]")}"
}

sealed trait Argument {
  def argumentType: Type

  def code: String
}

case class CallSiteRef(callSite: CallSite) extends Argument {
  val code: String = callSite.code

  override def argumentType: Type = callSite.declaration.returnType
}

case class ValueRef(symbol: s.SymbolInformation) extends Argument {
  val code: String = symbol.displayName

  override def argumentType: Type = ???
}

case class Literal(value: Any) extends Argument {
  val code: String = value.toString

  override def argumentType: Type = ???
}

case class Tuple(args: List[Argument]) extends Argument {
  val code: String = args.map(_.code).mkString("(", ",", ")")

  override def argumentType: Type = ???
}

case class CodeRef(range: s.Range) extends Argument {
  override def argumentType: Type = ???

  val code: String = s"code<${range.toString}>"
}

case class Placeholder() extends Argument {
  override def argumentType: Type = ???
  val code: String = "_"
}

case class ArgumentsList(args: Seq[Argument], syntactic: Boolean)

sealed trait CallSite {
  def declaration: Declaration

  def code: String

  def argss: Seq[ArgumentsList]

  def typeArgs: Seq[Type]

  // FIXME: a method to check class invariant rules

  // FIXME: check the last parameterList in the declaration and if it is implcit return the last the argument list
  def implicitArgs: Option[ArgumentsList] =
    declaration
      .parameterLists
      .find(_.isImplicit)
      .map(_ => argss.last)

  // FIXME: copy syntax does not work
  def update(
              declaration: Declaration = this.declaration,
              argss: Seq[ArgumentsList] = this.argss,
              typeArgs: Seq[Type] = this.typeArgs
            ): CallSite =
    this match {
      case x: NormalCall => x.copy(declaration = declaration, argss = argss, typeArgs = typeArgs)
      case x: SyntheticCall => x.copy(declaration = declaration, argss = argss, typeArgs = typeArgs)
      case x: ConversionCall => x.copy(declaration = declaration, argss = argss, typeArgs = typeArgs)
    }
}


case class NormalCall(
                       declaration: Declaration,
                       tree: Tree,
                       lhsRange: s.Range,
                       argss: Seq[ArgumentsList] = Seq(),
                       typeArgs: Seq[Type] = Seq()
                     ) extends CallSite {
  val code: String = tree.toString()
  val range: s.Range = tree.pos

  override def toString: String = s"NormalCall(${declaration.name}, ${tree.productPrefix}($range,$code)"
}

case class SyntheticCall(
                          declaration: Declaration,
                          range: Option[s.Range],
                          argss: Seq[ArgumentsList] = Seq(),
                          typeArgs: Seq[Type] = Seq()
                        ) extends CallSite {
  val code: String = {
    val argsStr = argss.map(_.args.map(_.code.mkString("(", ",", ")"))).mkString("")
    s"${declaration.name}${typeArgs.mkStringOpt("[", ",", "]")}$argsStr"
  }

  override def toString: String = s"SyntheticCall(${declaration.name}, ${range}, $code)"
}

case class ConversionCall(
                           declaration: Declaration,
                           range: s.Range,
                           argss: Seq[ArgumentsList] = Seq(),
                           typeArgs: Seq[Type] = Seq()
                         ) extends CallSite {

  val code: String = {
    val argsStr = argss.map(_.args.map(_.code.mkString("(", ",", ")"))).mkString("")
    s"${declaration.name}${typeArgs.mkStringOpt("[", ",", "]")}$argsStr"
  }

  override def toString: String = s"ConversionCall(${declaration.name})"
}

class CallSiteExtractor(val db: s.TextDocument, val symtab: SymbolTable) {

  import CallSiteExtractor._

  implicit val resolver = SemanticdbSymbolResolver(db, symtab)

  val tree: Tree = db.text.parse[Source].get
  val synthetics: Seq[s.Synthetic] = db.synthetics

  // TODO: move a function and make immutable
  val declarations = mutable.Map[String, Declaration]()

  private val extraction: (Seq[CallSite], Seq[Throwable]) = {
    val (callSites, errors) = extractExplicitCallSites(tree).split()
    val (updatedCallSites, syntheticErrors) = updateWithSynthetics(callSites).split()

    (updatedCallSites, errors ++ syntheticErrors)
  }

  val callSites: Seq[CallSite] = extraction._1

  val failures: Seq[Throwable] = extraction._2

  def resolveDeclaration(symbol: s.SymbolInformation): Declaration = {
    declarations.getOrElseUpdate(symbol.symbol, Declaration(symbol))
  }

  def findCallSiteFunctionSymbol(t: Term): Term.Name = t match {
    case Term.Select(_, name) => name
    case Term.ApplyType(fun, _) => findCallSiteFunctionSymbol(fun)
    case Term.Apply(fun, _) => findCallSiteFunctionSymbol(fun)
    case x: Term.Name => x
    case _ => throw new Exception(s"${t.structure} is not supported function name term")
  }

  def extractExplicitCallSites(tree: Tree): Seq[Try[CallSite]] = {

    def resolveType(t: scala.meta.Type): Type = t match {
      case Type.Name(_) =>
        TypeRef(resolver.resolveSymbol(t.pos))
      case Type.Apply(tpe, args) =>
        ParamTypeRef(resolveType(tpe), args map resolveType)
      case _ =>
        throw new Exception(s"Unsupported type tree: $t")
    }

    def createArgument(css: List[NormalCall])(arg: Term): Argument = {
      // TODO: do we actually need this - should be the first one right?
      def findCallsiteFor(t: Term): NormalCall =
        css.find(_.tree.pos == t.pos).getOrThrow(new Exception(s"Unable to find callsite for $t"))

      arg match {
        case x@(_: Term.Apply
                | _: Term.Select
                | _: Term.ApplyType
                | _: Term.ApplyInfix
                | _: Term.ApplyUnary
                | _: Term.New
                | _: Term.NewAnonymous
                | _: Term.Interpolate) =>
          val cs = findCallsiteFor(x)
          CallSiteRef(cs)

        case _: Term.Name =>
          ValueRef(resolver.resolveSymbol(arg.pos))

        case Term.Tuple(args) =>
          Tuple(args map createArgument(css))

        case Lit(value) =>
          Literal(value)

        case _:Term.Placeholder =>
          Placeholder()

        case _ =>
          throw new Exception(s"Unsupported argument term: ${arg.structure} (${arg.toString}) at ${arg.pos.toRange}")
      }
    }

    // TODO: ++ is too much - too many call sites potentially - should be Nil in args = process(Nil)
    def process(css: List[Try[NormalCall]])(tree: Tree): List[Try[NormalCall]] =
      tree match {
        case Term.Apply(fun, args) => {
          Try {
            val argsCss = args flatMap process(css)
            val arguments = args map createArgument(argsCss collect { case Success(x: NormalCall) => x })
            val argumentList = ArgumentsList(arguments, true)

            fun match {
              case x: Term.Name =>
                val funSymbol = resolver.resolveSymbol(x.pos)
                val declaration = resolveDeclaration(funSymbol)
                val cs = NormalCall(declaration, tree, fun.pos, argss = Seq(argumentList))

                Success(cs) :: (argsCss ++ css)
              case x =>
                val Success(y) :: ys = process(css)(x)
                val cs = y.copy(tree = tree, lhsRange = fun.pos, argss = y.argss :+ argumentList)

                Success(cs) :: (ys ++ argsCss) //  ++ css
            }
          } match {
            case Success(x) => x
            case Failure(x) => Failure(x) :: css
          }
        }

        case Term.ApplyType(fun, targs) => {
          Try {
            val typeArgs = targs map resolveType

            fun match {
              case x: Term.Name =>
                val funSymbol = resolver.resolveSymbol(x.pos)
                val declaration = resolveDeclaration(funSymbol)
                val cs = NormalCall(declaration, tree, fun.pos, typeArgs = typeArgs)

                Success(cs) :: css
              case x =>
                val Success(y) :: ys = process(css)(x)
                val cs = y.copy(tree = tree, lhsRange = fun.pos, typeArgs = typeArgs)

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
            val cs = NormalCall(declaration, tree, qual.pos)

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
            val arguments = args map createArgument(argsCss collect { case Success(x: NormalCall) => x })
            val argumentList = ArgumentsList(arguments, true)
            val r = {
              val lr = lhs.pos.toRange
              val or = op.pos.toRange
              s.Range(lr.startLine, lr.startCharacter, or.endLine, or.endCharacter)
            }
            val funSymbol = resolver.resolveSymbol(op.pos)
            val declaration = resolveDeclaration(funSymbol)
            val typeArgs = targs map resolveType
            val cs = NormalCall(declaration, tree, r, Seq(argumentList), typeArgs)

            Success(cs) :: (lhsCss ++ argsCss ++ css)
          } match {
            case Success(x) => x
            case Failure(x) => Failure(x) :: css
          }
        }

        case Term.ApplyUnary(op, arg) => {
          Try {
            val argCss = process(css)(arg)
            val argument = createArgument(argCss collect { case Success(x: NormalCall) => x })(arg)
            val argumentList = ArgumentsList(Seq(argument), true)
            val funSymbol = resolver.resolveSymbol(op.pos)
            val declaration = resolveDeclaration(funSymbol)
            val cs = NormalCall(declaration, tree, op.pos, Seq(argumentList))

            Success(cs) :: (argCss ++ css)
          } match {
            case Success(x) => x
            case Failure(x) => Failure(x) :: css
          }
        }

        case Term.Interpolate(prefix, _, args) => {
          Try {
            val argsCss = args flatMap process(css)
            val arguments = args map createArgument(argsCss collect { case Success(x: NormalCall) => x })
            val argumentsList = ArgumentsList(arguments, true)
            val funSymbol = resolver.resolveSymbol(prefix.pos)
            val declaration = resolveDeclaration(funSymbol)
            val r = {
              val tmp = prefix.pos.toRange
              tmp.copy(endLine = tmp.startLine, endCharacter = tmp.startCharacter)
            }
            val cs = NormalCall(declaration, tree, r, argss = Seq(argumentsList))

            Success(cs) :: (argsCss ++ css)
          } match {
            case Success(x) => x
            case Failure(x) => Failure(x) :: css
          }
        }

        case Term.New(Init(tpe, name, argss)) => {
          Try {
            val argsCss = argss.flatMap(_.flatMap(process(css)))
            val argumentss = argss map (args => args map createArgument(argsCss collect { case Success(x: NormalCall) => x }))
            val argumentsLists = argumentss map (ArgumentsList(_, true))

            // TODO: check lhsRange
            // TODO: return type

            val funSymbol = resolver.resolveSymbol(name.pos)
            val declaration = resolveDeclaration(funSymbol)
            val typeArgs = resolveType(tpe) match {
              case ParamTypeRef(_, args) => args
              case _ => Seq()
            }

            val cs = NormalCall(declaration, tree, name.pos, argumentsLists, typeArgs)

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
      if (symbol.kind == Kind.METHOD && (symbol.properties & (0x800 | 0x400)) == 0) {
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
      if (symbol.kind == Kind.METHOD && (symbol.properties & (0x800 | 0x400)) > 0) {
        Some(symbol)
      } else {
        None
      }
    }
  }

  final def createArgument(t: s.Tree, cs: Option[CallSite]): Argument =
    (cs, t) match {
      case (Some(x), idTreeMethod(_) | _: s.TypeApplyTree | _: s.SelectTree | _: s.ApplyTree) =>
        CallSiteRef(x)
      case (None, idTreeValue(symbol)) =>
        ValueRef(symbol)
      case (None, s.OriginalTree(Some(range))) =>
        CodeRef(range)
      case (_, s.FunctionTree(params, term)) =>
        Literal(t)
      case _ =>
        throw new Exception(s"Unsupported semanticdb argument $t")
    }

  final def resolveType(x: s.Type): Type = x match {
    case s.TypeRef(_, symbol, Seq()) =>
      TypeRef(resolver.resolveSymbol(symbol))
    case s.TypeRef(_, symbol, targs) =>
      ParamTypeRef(TypeRef(resolver.resolveSymbol(symbol)), targs map resolveType)
    case s.Type.Empty =>
      Empty
    case _ =>
      throw new Exception(s"Unsupported semanticdb type $x")
  }

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

  def updateWithSynthetics(explicitCallsites: Seq[CallSite]): Seq[Try[CallSite]] = {
    val callSitesIndex = explicitCallsites.to[mutable.ArrayBuffer]
    val errors = mutable.ArrayBuffer[Throwable]()

    def process(node: s.Tree): Option[Int] = node match {
      // implicit conversion
      case s.ApplyTree(fn, Seq(from@s.OriginalTree(Some(range)))) => {
        val resolvedFun = resolveName(fn)
        val declaration = resolveDeclaration(resolvedFun)
        val cs = ConversionCall(declaration, range, Seq(ArgumentsList(Seq(createArgument(from, None)), false)))
        callSitesIndex += cs

        process(fn)
      }

      // implicit arguments (e.g. `(Seq(1) ++ Seq(2))(canBuildFrom)`)
      case s.ApplyTree(fun, implicitArgs) => {
        val idx = fun match {
          case s.OriginalTree(Some(range)) =>
            val idx = callSitesIndex.lastIndexWhere {
              case NormalCall(_, t, _, _, _) => t.pos.toRange == range
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
        val resolvedArgs = (implicitArgs zip (argsCss map (_ map callSitesIndex))) map (x => createArgument(x._1, x._2))
        val args = ArgumentsList(resolvedArgs, false)
        val cs = callSitesIndex(idx)
        callSitesIndex.update(
          idx,
          cs.update(argss = cs.argss :+ args)
        )

        Some(idx)
      }

      // inferred type arguments (e.g. `Seq(1)` -> `Seq[Int](1)`)
      case s.TypeApplyTree(fun, targs) => {
        val idx = fun match {
          case s.OriginalTree(Some(range)) =>
            val idx = callSitesIndex.lastIndexWhere {
              case NormalCall(_, _, r, _, _) => r == range
              case _ => false
            }
            if (idx == -1) {
              throw new Exception(s"Unable to find call site for TypeApplyTree $fun in $node")
            }
            idx

          case _ =>
            process(fun).getOrThrow(new Exception(s"Unable to find call site for TypeApplyTree $node"))
        }

        val typeArgs = targs map resolveType
        val cs = callSitesIndex(idx)
        callSitesIndex.update(
          idx,
          cs.update(typeArgs = typeArgs)
        )

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
                case NormalCall(_, _, r, _, _) => r == range
                case _ => false
              } match {
                case -1 =>
                  // case for .map, .flatMap, ...
                  // we need to create a synthetic call
                  val cs = SyntheticCall(declaration, None)
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
          cs.update(declaration = declaration)
        )

        Some(idx)
      }

      case idTreeMethod(symbol) => {
        val declaration = resolveDeclaration(symbol)
        val cs = SyntheticCall(declaration, None)

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
