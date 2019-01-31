package cz.cvut.fit.prl.scala.implicits.extractor
import org.scalatest.Matchers

class SymtabSuite extends SemanticdbSuite with Matchers {

  test("symbol resolution") {
    val symtab = GlobalSymbolTable(classpath)

    symtab.resolve("java/util/ArrayList#size.") shouldBe defined
    symtab.resolve("java/lang/String#") shouldBe defined
    //symtab.resolve("java/lang/String#`+`().") shouldBe defined
    symtab.resolve("scala/Predef.ArrowAssoc#`->`().") shouldBe defined
    symtab.resolve("scala/collection/generic/GenericCompanion#apply().") shouldBe defined
    symtab.resolve("scala/collection/") shouldBe defined
    symtab.resolve("_root_/") shouldBe defined
    symtab.resolve("_empty_/") shouldBe defined
  }

}
