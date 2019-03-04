package cz.cvut.fit.prl.scala.implicits.extractor

import cz.cvut.fit.prl.scala.implicits.model._
import cz.cvut.fit.prl.scala.implicits.model.Type
import cz.cvut.fit.prl.scala.implicits.utils._

import scala.collection.mutable
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.{semanticdb => s}
import scala.util.Try

class ExtractionContext(
    moduleId: String,
    resolver: SemanticdbSymbolResolver,
    val sourcePaths: List[String])
    extends TypeResolver
    with SymbolResolver {

  private val declarationIndex: mutable.Map[String, Declaration] = mutable.Map()
  private var callSiteId: Int = 0

  private val sourcePathIndex: mutable.Map[String, Option[String]] = mutable.Map()

  def resolveDeclaration(symbol: String): Declaration =
    resolveDeclaration(resolveSymbol(symbol))

  def declarations: Iterable[Declaration] = declarationIndex.values.toList

  def createCallSiteId: Int = {
    callSiteId = callSiteId + 1
    callSiteId
  }

  def sourcePath(uri: String): Option[String] = {
    sourcePathIndex.getOrElseUpdate(
      uri,
      sourcePaths.collectFirst { case x if uri.startsWith(x) => x }
    )
  }

  def relaxedSourcePath(uri: String): String = sourcePath(uri).getOrElse("")

  override def resolveType(ref: DeclarationRef): Declaration = {
    assert(ref.moduleId == moduleId)

    declarationIndex(ref.declarationFqn)
  }

  override def resolveSymbol(symbol: String): ResolvedSymbol =
    resolver.resolveSymbol(symbol)

  override def resolveSymbol(unit: String, range: s.Range): ResolvedSymbol =
    resolver.resolveSymbol(unit, range)

  private def resolveSymbolInfo(symbol: String): s.SymbolInformation =
    resolver.resolveSymbol(symbol).symbolInfo

  private def resolveDeclaration(resolvedSymbol: ResolvedSymbol): Declaration = {
    val symbolInfo = resolvedSymbol.symbolInfo
    declarationIndex.get(symbolInfo.symbol) match {
      case Some(x) => x
      case None =>
        // this will be just a temporary one to prevent infinite
        // loop in the case a TypeSignature contains a cycle
        // (e.g. class X[A <: X[A]])
        // The actual value of the key will be replaced later, but
        // that should be safe since we only call .ref which is
        // fixed from the symbol
        try {
          val prototype =
            createDeclaration(symbolInfo).copy(location = resolvedSymbol.location)

          declarationIndex += symbolInfo.symbol -> prototype

          val signature = createSignature(symbolInfo.signature)
          val declaration = prototype.withSignature(signature)

          declarationIndex += symbolInfo.symbol -> declaration
          declaration
        } catch {
          case e: Throwable =>
            declarationIndex -= symbolInfo.symbol
            throw e
        }
    }
  }

  private def createTypeSignature(
      signature: s.TypeSignature
  ): TypeSignature = {

    val typeParameters =
      signature.typeParameters.symbols.map(createTypeParameter)

    TypeSignature(typeParameters = typeParameters)
  }

  private def createTypeSignature(
      signature: s.ClassSignature
  ): TypeSignature = {
    val typeParameters =
      signature.typeParameters.symbols.map(createTypeParameter)
    val parents = signature.parents.map(x => createType(x, includeTopBottom = false)).filterNot(_.isEmpty)

    TypeSignature(typeParameters = typeParameters, parents = parents)
  }

  private def createMethodSignature(
      signature: s.MethodSignature
  ): MethodSignature = {

    val typeParameters =
      signature.typeParameters.symbols.map(createTypeParameter)
    val parameterLists = signature.parameterLists.map(createParameterList)
    val returnType = createType(signature.returnType, includeTopBottom = false)

    MethodSignature(
      typeParameters = typeParameters,
      parameterLists = parameterLists,
      returnType = returnType
    )
  }

  private def createValueSignature(signature: s.ValueSignature): ValueSignature = {
    ValueSignature(createType(signature.tpe, includeTopBottom = false))
  }

  private def createSignature(signature: s.Signature): Declaration.Signature =
    signature match {
      case x: s.TypeSignature =>
        Declaration.Signature.Type(createTypeSignature(x))
      case x: s.ClassSignature =>
        Declaration.Signature.Type(createTypeSignature(x))
      case x: s.MethodSignature =>
        Declaration.Signature.Method(createMethodSignature(x))
      case x: s.ValueSignature =>
        Declaration.Signature.Value(createValueSignature(x))
      case _ =>
        throw new UnsupportedElementException("signature", signature.getClass.getSimpleName)
    }

  private def createDeclaration(
      symbolInfo: s.SymbolInformation
  ): Declaration = {
    import Declaration.Kind._

    val location = null
    val kind = symbolInfo match {
      case x if x.isVal => VAL
      case x if x.isVar => VAR
      case x if x.isType => TYPE
      case x if x.isClass => CLASS
      case x if x.isTrait => TRAIT
      case x if x.isObject => OBJECT
      case x if x.isInterface => INTERFACE
      case x if x.isEnum => ENUM
      case x if x.isMethod => DEF
      case x if x.isConstructor => DEF
      case x if x.isTypeParameter => TYPE_PARAMETER
      case x if x.isMacro => MACRO
      case x if x.isParameter => PARAMETER
      case x => throw new UnsupportedElementException("declaration symbol kind", x.kind)
    }

    Declaration(
      declarationId = symbolInfo.symbol,
      moduleId = moduleId,
      kind = kind,
      name = symbolInfo.displayName,
      location = location,
      language = symbolInfo.language,
      isImplicit = symbolInfo.isImplicit
    )
  }

  private def createParameterList(scope: s.Scope): ParameterList = {
    val parameters = scope.symbols.map(resolveSymbolInfo).map(createParameter)
    ParameterList(parameters)
  }

  private def createParameter(symbolInfo: s.SymbolInformation): Parameter =
    symbolInfo.signature match {
      case s.ValueSignature(tpe) =>
        Parameter(
          symbolInfo.displayName,
          createType(tpe, includeTopBottom = false),
          symbolInfo.isImplicit
        )
      case x =>
        throw new UnexpectedElementException("parameter signature", x.getClass.getSimpleName)
    }

  private def createTypeParameter(symbol: String): TypeParameter =
    createTypeParameter(resolveSymbolInfo(symbol))

  private def createTypeParameter(
      symbolInfo: s.SymbolInformation
  ): TypeParameter = symbolInfo.signature match {
    case s.TypeSignature(typeParameters, lowerBound, upperBound) =>
      TypeParameter(
        name = symbolInfo.displayName,
        typeParameters = typeParameters.symbols.map(createTypeParameter),
        lowerBound = Try(createType(lowerBound, includeTopBottom = false)).getOrElse(Type.Empty),
        upperBound = Try(createType(upperBound, includeTopBottom = false)).getOrElse(Type.Empty)
      )
    case x =>
      throw new UnexpectedElementException("type parameter signature", x.getClass.getSimpleName)
  }

  def createType(tpe: s.Type, includeTopBottom: Boolean): Type = tpe match {
    case x: s.TypeRef if !includeTopBottom && x.isTopOrBottom => Type.Empty
    // TODO: do we need to do anything prefix?
    case s.TypeRef(_, symbol, typeArguments) =>
      createTypeReference(symbol, typeArguments)
    case s.AnnotatedType(annotations, t) =>
      // TODO: annotation
      createType(t, includeTopBottom)
    case s.SingleType(prefix, symbol) =>
      val parent = resolveDeclaration(symbol)
      TypeRef(parent.declarationId)
    case s.ByNameType(t) =>
      createType(t, includeTopBottom)
    case s.RepeatedType(repeatedType) =>
      createTypeReference("scala/Array#", Seq(repeatedType))
    case s.ExistentialType(t, _) =>
      // TODO: is this ok?
      createType(t, includeTopBottom)
    case s.UniversalType(_, t) =>
      // TODO: is this ok?
      createType(t, includeTopBottom)
    case s.StructuralType(t, _) =>
      createType(t, includeTopBottom)
    case s.WithType(types) =>
      // TODO: is this ok?
      createType(types.head, includeTopBottom)
    case s.ThisType(symbol) =>
      TypeRef(resolveDeclaration(symbol).declarationId)
    case s.Type.Empty =>
      Type.Empty
    case x =>
      throw new UnsupportedElementException("type", x.getClass.getSimpleName)
  }

  def createTypeArguments(typeArguments: Seq[s.Type], includeTopBottom: Boolean): List[Type] = {
    // TODO: this one will silently ignore some type resolution errors
    typeArguments
      .filter {
        case s.TypeRef(_, symbol, _) if symbol.isLocal => false
        case _ => true
      }
      .map(x => Try(createType(x, includeTopBottom)))
      .collect {
        case scala.util.Success(x) => x
      }
      .toList
  }

  private def createTypeReference(symbol: String, typeArguments: Seq[s.Type]): Type = {
    resolveSymbolInfo(symbol) match {
      case x if x.symbol.isLocal =>
        throw new UnsupportedElementException("TypeRef type", "local")
      case x if x.isTypeParameter =>
        val parent = resolveDeclaration(symbol.owner)
        TypeParameterRef(
          parent.declarationId,
          x.displayName,
          typeArguments.map(x => createType(x, includeTopBottom = false))
        )
      case x if x.isType || x.isClass || x.isTrait || x.isInterface || x.isObject =>
        val parent = resolveDeclaration(symbol)
        TypeRef(parent.declarationId, createTypeArguments(typeArguments, includeTopBottom = false))
      case x =>
        throw new UnsupportedElementException("TypeRef type", x)
    }
  }
}
