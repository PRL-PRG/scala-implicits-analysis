package cz.cvut.fit.prl.scala.implicits.extractor
import cz.cvut.fit.prl.scala.implicits.model.{Location, Position}
import org.scalatest.{FunSuite, Matchers, OptionValues}

import scala.meta.internal.semanticdb.Language.SCALA
import scala.meta.internal.semanticdb.SymbolInformation.Kind.{METHOD, PARAMETER}
import scala.meta.internal.semanticdb.{MethodSignature, Scope, TypeRef, TypeSignature}
import scala.meta.internal.{semanticdb => s}

class SemanticdbSymbolResolverSuite extends FunSuite with Matchers {

  class SymbolTableStub(table: Map[String, ResolvedSymbol]) extends SymbolTable {
    override def resolve(symbol: String): Option[ResolvedSymbol] = table.get(symbol)
  }

  test("resolver should fix location of evidence$n parameters") {
    val localSymbols = Map(
      "A.f()." -> ResolvedSymbol(
        s.SymbolInformation("A.f().", SCALA, METHOD, 32, "f", MethodSignature()),
        Some(Location("path1", "uri1", Some(Position(1, 2, 1, 4))))
      )
    )
    val classSymbols = Map(
      "A.f().(evidence$1)" -> ResolvedSymbol(
        s.SymbolInformation(
          "A.f().(evidence$1)",
          SCALA,
          PARAMETER,
          32,
          "evidence$1",
          TypeSignature()),
        Some(Location("path2", "uri2", None))
      )
    )
    val symtab = new SymbolTableStub(classSymbols)
    val resolver = new SemanticdbSymbolResolver(localSymbols, Map(), symtab)

    val symbol = resolver.resolveSymbol("A.f().(evidence$1)")
    symbol.location.get shouldBe Location("path2", "uri2", Some(Position(1, 4, 1, 4)))
  }

}
