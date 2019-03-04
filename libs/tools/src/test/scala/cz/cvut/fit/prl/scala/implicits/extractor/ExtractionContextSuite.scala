package cz.cvut.fit.prl.scala.implicits.extractor

import cz.cvut.fit.prl.scala.implicits.model.{DeclarationRef, TypeResolver}
import cz.cvut.fit.prl.scala.implicits.model.ModelDSL._
import cz.cvut.fit.prl.scala.implicits.{model => m}
import cz.cvut.fit.prl.scala.implicits.utils._
import org.scalatest.{Inside, Matchers, OptionValues}

import scala.language.implicitConversions
import scala.meta.internal.{semanticdb => s}
import scala.meta._
import scala.meta.io.Classpath

// TODO: rewrite using scrap your boilerplate pattern
trait ModelSimplification {

  implicit class SimplifyCallSite(that: m.CallSite) {

    def simplify(ctx: ExtractionContext): m.CallSite = that.copy(
      location = that.location.simplify(ctx),
      typeArguments = that.typeArguments.map(_.simplify),
      implicitArgumentTypes = that.implicitArgumentTypes
    )
  }

  implicit class SimplifyDeclaration(that: m.Declaration) {

    def simplify(ctx: ExtractionContext): m.Declaration =
      that.copy(
        location = that.location.simplify(ctx),
        signature = that.signature.simplify
      )
  }
  implicit class SimplifySignature(that: m.Declaration.Signature) {

    def simplify: m.Declaration.Signature = that match {
      case m.Declaration.Signature.Method(v) =>
        m.Declaration.Signature.Method(v.simplify)
      case m.Declaration.Signature.Type(v) => m.Declaration.Signature.Type(v.simplify)
      case y => y
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
        y.copy(typeArguments = y.typeArguments.map(_.simplify))
      case y => y
    }
  }

  implicit class SimplifyTypeRef(that: m.TypeRef) {

    def simplify: m.TypeRef =
      that.copy(
        typeArguments = that.typeArguments.map(_.simplify)
      )
  }

  implicit class SimplifyLocation(that: m.Location) {

    def simplify(ctx: ExtractionContext): m.Location = {
      that match {
        case m.Location(path, _, _) if ctx.sourcePaths.contains(path) =>
          TestLocalLocation
        case m.Location("", _, _) =>
          TestLocalLocation
        case _ =>
          TestExternalLocation
      }
    }
  }
}

case class DeclarationsResult(
    declarations: Seq[m.Declaration],
    originalDeclarations: Seq[m.Declaration],
    failures: Seq[Throwable],
    ctx: ExtractionContext,
    db: s.TextDocument)
    extends TypeResolver {

  override def resolveType(ref: DeclarationRef): m.Declaration = ctx.resolveType(ref)
}

object DeclarationsResult extends ModelSimplification {

  def apply(ctx: ExtractionContext, db: s.TextDocument): DeclarationsResult = {
    val extractor = new DeclarationExtractor(ctx)
    val result = extractor.extractImplicitDeclarations(db)
    val (declarations, failures) = result.split()

    DeclarationsResult(declarations.map(_.simplify(ctx)), declarations, failures, ctx, db)
  }
}

case class CallSitesResult(
    callSites: Seq[m.CallSite],
    originalCallSites: Seq[m.CallSite],
    failures: Seq[Throwable],
    callSitesCount: Int,
    ctx: ExtractionContext,
    db: s.TextDocument)
    extends TypeResolver {

  override def resolveType(ref: DeclarationRef): m.Declaration = ctx.resolveType(ref)
}

object CallSitesResult extends ModelSimplification {

  def apply(ctx: ExtractionContext, db: s.TextDocument): CallSitesResult = {
    val ast = db.text.parse[Source].get
    val extractor = new CallSiteExtractor(ctx)
    val result = extractor.extractImplicitCallSites(TestModuleId, db, ast)
    val (callSites, failures) = result.split()
    val count = extractor.callSiteCount(ast)

    CallSitesResult(callSites.map(_.simplify(ctx)), callSites, failures, count, ctx, db)
  }
}

abstract class ExtractionContextSuite
    extends SemanticdbSuite
    with Matchers
    with OptionValues
    with Inside
    with CaseClassAssertions
    with ModelSimplification {

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

    database(name, code) { (outputPath, db) =>
      val symtab = new GlobalSymbolTable(classpath ++ Classpath(outputPath.path), outputPath.path)
      val sourcePaths = List(outputPath.pathAsString)
      val resolver = SemanticdbSymbolResolver(Seq(db), symtab, sourcePaths)
      val ctx = new ExtractionContext(TestModuleId, resolver, sourcePaths)

      fn(ctx, db)
    }
  }

  def declarations(
      name: String,
      code: String
  )(fn: DeclarationsResult => Unit): Unit = {
    extraction(name, code) { (ctx, db) =>
      val result = DeclarationsResult(ctx, db)
      checkNoFailures(result.failures)
      fn(result)
    }
  }

  def callSites(
      name: String,
      code: String
  )(fn: CallSitesResult => Unit): Unit = {
    extraction(name, code) { (ctx, db) =>
      val result = CallSitesResult(ctx, db)
      checkNoFailures(result.failures)
      fn(result)
    }
  }

  def implicits(
      name: String,
      code: String
  )(fn: (DeclarationsResult, CallSitesResult) => Unit): Unit = {
    extraction(name, code) { (ctx, db) =>
      val declResult = DeclarationsResult(ctx, db)
      checkNoFailures(declResult.failures)

      val csResult = CallSitesResult(ctx, db)
      checkNoFailures(csResult.failures)

      fn(declResult, csResult)
    }
  }

  def checkNoFailures(failures: Traversable[Throwable]): Unit = {
    failures.foreach(_.printStackTrace())
    failures shouldBe empty
  }

}
