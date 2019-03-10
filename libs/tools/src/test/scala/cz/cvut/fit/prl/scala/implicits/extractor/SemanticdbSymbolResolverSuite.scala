package cz.cvut.fit.prl.scala.implicits.extractor

import cz.cvut.fit.prl.scala.implicits.model.{Location, Position}
import org.scalatest.{FunSuite, Matchers}

import scala.meta.internal.semanticdb.Language.SCALA
import scala.meta.internal.semanticdb.SymbolInformation.Kind.{METHOD, PARAMETER}
import scala.meta.internal.semanticdb.{MethodSignature, TypeSignature}
import scala.meta.internal.{semanticdb => s}

class SemanticdbSymbolResolverSuite extends FunSuite with Matchers {

  class SymbolTableStub(table: Map[String, ResolvedSymbol]) extends SymbolTable {
    override def resolve(symbol: String): Option[ResolvedSymbol] = table.get(symbol)
  }

  test("resolver should fix location of evidence$n parameters") {
    val classSymbols: Map[String, ResolvedSymbol] = Map(
      "A.f().(evidence$1)" -> ResolvedSymbol(
        s.SymbolInformation(
          symbol = "A.f().(evidence$1)",
          language = SCALA,
          kind = PARAMETER,
          properties = 32,
          displayName = "evidence$1",
          signature = TypeSignature()
        ),
        Location("path2", "uri2", None)
      )
    )

    val localSymbols = Map(
      "A.f()." -> ResolvedSymbol(
        s.SymbolInformation(
          symbol = "A.f().",
          language = SCALA,
          kind = METHOD,
          properties = 32,
          displayName = "f",
          signature = MethodSignature()
        ),
        Location("path1", "uri1", Some(Position(1, 2, 1, 4)))
      )
    )

    val symtab = new SymbolTableStub(classSymbols)
    val resolver = new SemanticdbSymbolResolver(localSymbols, List(), symtab)
    implicit val db = s.TextDocument()

    val symbol = resolver.resolveSymbol("A.f().(evidence$1)")

    symbol.location shouldBe Location("path2", "uri2", Some(Position(1, 4, 1, 4)))
  }

}
