package cz.cvut.fit.prl.scala.implicits

import cz.cvut.fit.prl.scala.implicits.extractor.{
  CallSiteExtractor,
  DeclarationExtractor,
  ExtractionContext,
  SemanticdbSymbolResolver
}
import cz.cvut.fit.prl.scala.implicits.model.Declaration.Signature
import cz.cvut.fit.prl.scala.implicits.model._
import cz.cvut.fit.prl.scala.implicits.utils._
import org.scalatest.{Inside, Matchers, OptionValues}

import scala.language.implicitConversions
import scala.meta.internal.{semanticdb => s}

case class DeclarationsResult(
    declarations: Seq[Declaration],
    ctx: ExtractionContext,
    db: s.TextDocument)
    extends TypeResolver {
  override def resolveType(tpe: Type): Declaration = ctx.resolveType(tpe)
}

case class CallSitesResult(
    callSites: Seq[CallSite],
    originalcallSites: Seq[CallSite],
    callSitesCount: Int,
    ctx: ExtractionContext,
    db: s.TextDocument)
    extends TypeResolver {
  override def resolveType(tpe: Type): Declaration = ctx.resolveType(tpe)
}

abstract class ExtractionContextSuite
    extends SemanticdbSuite
    with Matchers
    with OptionValues
    with Inside
    with CaseClassAssertions {

  val TestLocalLocation = Local("test-location", Position(0, 0, 0, 0))
  val TestExternalLocation = External(false, "test-path", "test-entry")

  def extraction(
      name: String,
      code: String
  )(fn: (ExtractionContext, s.TextDocument) => Unit): Unit = {
    implicit object RangeSorted extends Ordering[s.Range] {
      override def compare(x: s.Range, y: s.Range): Int = {
        val d = x.startLine.compare(y.startLine)
        if (d == 0) x.startCharacter.compare(y.startCharacter) else d
      }
    }

    database(name, code) { db =>
      val resolver = SemanticdbSymbolResolver(Seq(db), symtab)
      val ctx = new ExtractionContext(resolver)
      val extractor = new DeclarationExtractor(ctx)
      fn(ctx, db)
    }
  }

  def declarations(
      name: String,
      code: String
  )(fn: DeclarationsResult => Unit): Unit = {
    extraction(name, code) { (ctx, db) =>
      val extractor = new DeclarationExtractor(ctx)
      val result = extractor.extractImplicitDeclarations(db)
      val (declarations, failures) = result.split()

      checkNoFailures(failures)
      fn(DeclarationsResult(declarations.map(_.simplify), ctx, db))
    }
  }

  def callSites(
      name: String,
      code: String
  )(fn: CallSitesResult => Unit): Unit = {
    extraction(name, code) { (ctx, db) =>
      val extractor = new CallSiteExtractor(ctx)
      val result = extractor.extractImplicitCallSites(db)
      val (callSites, failures) = result.split()
      val count = extractor.callSiteCount(db)

      checkNoFailures(failures)
      fn(CallSitesResult(callSites.map(_.simplify), callSites, count, ctx, db))
    }
  }

  def checkNoFailures(failures: List[Throwable]): Unit = {
    failures.foreach(_.printStackTrace())
    failures shouldBe empty
  }

  implicit class SimplifyCallSite(that: CallSite) {

    def simplify: CallSite = that.copy(
      ref = that.ref.simplify,
      location = that.location.simplify,
      typeArguments = that.typeArguments.map(_.simplify),
      implicitArgumentTypes = that.implicitArgumentTypes.map(_.simplify)
    )
  }

  implicit class SimplifyDeclaration(that: Declaration) {

    def simplify: Declaration =
      that.copy(
        location = that.location.simplify,
        signature = that.signature.simplify
      )
  }
  implicit class SimplifySignature(that: Signature) {

    def simplify: Signature = that match {
      case y @ Signature.Method(v) => Signature.Method(v.simplify)
      case y @ Signature.Type(v)   => Signature.Type(v.simplify)
      case y                       => y
    }
  }

  implicit class SimplifyMethodSignature(that: MethodSignature) {

    def simplify: MethodSignature = that.copy(
      typeParameters = that.typeParameters.map(_.simplify),
      parameterLists = that.parameterLists.map(_.simplify),
      returnType = that.returnType.simplify
    )
  }

  implicit class SimplifyTypeSignature(that: TypeSignature) {

    def simplify: TypeSignature = that.copy(
      parents = that.parents.map(_.simplify),
      typeParameters = that.typeParameters.map(_.simplify)
    )
  }

  implicit class SimplifyParameterList(that: ParameterList) {
    def simplify: ParameterList = that.copy(that.parameters.map(_.simplify))
  }

  implicit class SimplifyParameter(that: Parameter) {
    def simplify: Parameter = that.copy(tpe = that.tpe.simplify)
  }

  implicit class SimplifyTypeParameter(that: TypeParameter) {

    def simplify: TypeParameter =
      that.copy(
        typeParameters = that.typeParameters.map(_.simplify),
        upperBound = that.upperBound.simplify,
        lowerBound = that.lowerBound.simplify)
  }

  implicit class SimplifyType(that: Type) {

    def simplify: Type = that match {
      case y: TypeRef => y.simplify
      case y: TypeParameterRef =>
        y.copy(ref = y.ref.simplify, typeArguments = y.typeArguments.map(_.simplify))
      case y => y
    }
  }

  implicit class SimplifyTypeRef(that: TypeRef) {

    def simplify: TypeRef =
      that.copy(
        ref = that.ref.simplify,
        typeArguments = that.typeArguments.map(_.simplify))

  }

  implicit class SimplifyDeclarationRef(that: DeclarationRef) {
    def simplify: DeclarationRef = that.copy(location = that.location.simplify)
  }

  implicit class SimplifyLocation(that: Location) {

    def simplify: Location = that match {
      case y: External => TestExternalLocation
      case y: Local    => TestLocalLocation
      case y           => y
    }
  }

  implicit def typeSignature2type(x: TypeSignature): Signature.Type =
    Signature.Type(x)

  implicit def methodSignature2method(x: MethodSignature): Signature.Method =
    Signature.Method(x)

}
