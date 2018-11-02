package cz.cvut.fit.prl.scala.implicits

import cz.cvut.fit.prl.scala.implicits.extractor.{
  CallSiteExtractor,
  DeclarationExtractor,
  ExtractionContext,
  SemanticdbSymbolResolver
}
import cz.cvut.fit.prl.scala.implicits.model.TypeResolver
import cz.cvut.fit.prl.scala.implicits.{model => m}
import cz.cvut.fit.prl.scala.implicits.utils._
import org.scalatest.{Inside, Matchers, OptionValues}

import scala.language.implicitConversions
import scala.meta.internal.{semanticdb => s}
import scala.meta._

case class DeclarationsResult(
    declarations: Seq[m.Declaration],
    ctx: ExtractionContext,
    db: s.TextDocument)
    extends TypeResolver {
  override def resolveType(tpe: m.Type): m.Declaration = ctx.resolveType(tpe)
}

case class CallSitesResult(
    callSites: Seq[m.CallSite],
    originalcallSites: Seq[m.CallSite],
    callSitesCount: Int,
    ctx: ExtractionContext,
    db: s.TextDocument)
    extends TypeResolver {
  override def resolveType(tpe: m.Type): m.Declaration = ctx.resolveType(tpe)
}

abstract class ExtractionContextSuite
    extends SemanticdbSuite
    with Matchers
    with OptionValues
    with Inside
    with CaseClassAssertions {

  val TestLocalLocation = m.Local("test-location", m.Position(0, 0, 0, 0))
  val TestExternalLocation = m.External(false, "test-path", "test-entry")

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
      val ast = db.text.parse[Source].get
      val extractor = new CallSiteExtractor(ctx)
      val result = extractor.extractImplicitCallSites(db, ast)
      val (callSites, failures) = result.split()
      val count = extractor.callSiteCount(ast)

      checkNoFailures(failures)
      fn(CallSitesResult(callSites.map(_.simplify), callSites, count, ctx, db))
    }
  }

  def checkNoFailures(failures: List[Throwable]): Unit = {
    failures.foreach(_.printStackTrace())
    failures shouldBe empty
  }

  implicit class SimplifyCallSite(that: m.CallSite) {

    def simplify: m.CallSite = that.copy(
      ref = that.ref.simplify,
      location = that.location.simplify,
      typeArguments = that.typeArguments.map(_.simplify),
      implicitArgumentTypes = that.implicitArgumentTypes.map(_.simplify)
    )
  }

  implicit class SimplifyDeclaration(that: m.Declaration) {

    def simplify: m.Declaration =
      that.copy(
        location = that.location.simplify,
        signature = that.signature.simplify
      )
  }
  implicit class SimplifySignature(that: m.Declaration.Signature) {

    def simplify: m.Declaration.Signature = that match {
      case y @ m.Declaration.Signature.Method(v) =>
        m.Declaration.Signature.Method(v.simplify)
      case y @ m.Declaration.Signature.Type(v) => m.Declaration.Signature.Type(v.simplify)
      case y                       => y
    }
  }

  implicit class SimplifyMethodSignature(that: m.MethodSignature) {

    def simplify: m.MethodSignature = that.copy(
      typeParameters = that.typeParameters.map(_.simplify),
      parameterLists = that.parameterLists.map(_.simplify),
      returnType = that.returnType.simplify
    )
  }

  implicit class SimplifyTypeSignature(that: m.TypeSignature) {

    def simplify: m.TypeSignature = that.copy(
      parents = that.parents.map(_.simplify),
      typeParameters = that.typeParameters.map(_.simplify)
    )
  }

  implicit class SimplifyParameterList(that: m.ParameterList) {
    def simplify: m.ParameterList = that.copy(that.parameters.map(_.simplify))
  }

  implicit class SimplifyParameter(that: m.Parameter) {
    def simplify: m.Parameter = that.copy(tpe = that.tpe.simplify)
  }

  implicit class SimplifyTypeParameter(that: m.TypeParameter) {

    def simplify: m.TypeParameter =
      that.copy(
        typeParameters = that.typeParameters.map(_.simplify),
        upperBound = that.upperBound.simplify,
        lowerBound = that.lowerBound.simplify)
  }

  implicit class SimplifyType(that: m.Type) {

    def simplify: m.Type = that match {
      case y: m.TypeRef => y.simplify
      case y: m.TypeParameterRef =>
        y.copy(ref = y.ref.simplify, typeArguments = y.typeArguments.map(_.simplify))
      case y => y
    }
  }

  implicit class SimplifyTypeRef(that: m.TypeRef) {

    def simplify: m.TypeRef =
      that.copy(
        ref = that.ref.simplify,
        typeArguments = that.typeArguments.map(_.simplify))

  }

  implicit class SimplifyDeclarationRef(that: m.DeclarationRef) {
    def simplify: m.DeclarationRef = that.copy(location = that.location.simplify)
  }

  implicit class SimplifyLocation(that: m.Location) {

    def simplify: m.Location = that match {
      case y: m.External => TestExternalLocation
      case y: m.Local    => TestLocalLocation
      case y           => y
    }
  }

  implicit def typeSignature2type(x: m.TypeSignature): m.Declaration.Signature.Type =
    m.Declaration.Signature.Type(x)

  implicit def methodSignature2method(
      x: m.MethodSignature): m.Declaration.Signature.Method =
    m.Declaration.Signature.Method(x)

}
