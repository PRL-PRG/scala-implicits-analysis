package cz.cvut.fit.prl.scala.implicits.extractor

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
    originalCallSites: Seq[m.CallSite],
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

  val TestLocalLocation = m.Location("test-location", "", Some(m.Position(0, 0, 0, 0)))
  val TestExternalLocation = m.Location("test-external-location", "", None)

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
      val symtab = GlobalSymbolTable(classpath ++ Classpath(outputPath.path))
      val sourcePaths = List(outputPath.pathAsString)
      val resolver = SemanticdbSymbolResolver(Seq(db), symtab, sourcePaths)
      val ctx = new ExtractionContext(resolver, sourcePaths)

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
      fn(DeclarationsResult(declarations.map(_.simplify(ctx)), ctx, db))
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
      fn(CallSitesResult(callSites.map(_.simplify(ctx)), callSites, count, ctx, db))
    }
  }

  def checkNoFailures(failures: List[Throwable]): Unit = {
    failures.foreach(_.printStackTrace())
    failures shouldBe empty
  }

  implicit class SimplifyCallSite(that: m.CallSite) {

    def simplify(ctx: ExtractionContext): m.CallSite = that.copy(
      location = that.location.map(_.simplify(ctx)),
      typeArguments = that.typeArguments.map(_.simplify),
      implicitArgumentTypes = that.implicitArgumentTypes.map(_.simplify)
    )
  }

  implicit class SimplifyDeclaration(that: m.Declaration) {

    def simplify(ctx: ExtractionContext): m.Declaration =
      that.copy(
        location = that.location.map(_.simplify(ctx)),
        signature = that.signature.simplify
      )
  }
  implicit class SimplifySignature(that: m.Declaration.Signature) {

    def simplify: m.Declaration.Signature = that match {
      case y @ m.Declaration.Signature.Method(v) =>
        m.Declaration.Signature.Method(v.simplify)
      case y @ m.Declaration.Signature.Type(v) => m.Declaration.Signature.Type(v.simplify)
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
        // coming from a class file from the locally compiled project
        case m.Location(path, _, _) if ctx.sourcePaths.contains(path) =>
          TestLocalLocation
        // coming from a local symbol from sdb.symbols
        case m.Location("", _, _) =>
          TestLocalLocation
        case _ => TestExternalLocation
      }
    }
  }

  implicit def typeSignature2type(x: m.TypeSignature): m.Declaration.Signature.Type =
    m.Declaration.Signature.Type(x)

  implicit def methodSignature2method(x: m.MethodSignature): m.Declaration.Signature.Method =
    m.Declaration.Signature.Method(x)

}
