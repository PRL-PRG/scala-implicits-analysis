package cz.cvut.fit.prl.scala.implicits

import org.scalatest.Matchers

class SymtabSuite extends SemanticdbSuite with Matchers {

  test("symbol resolution") {
    symtab.resolve("java/util/ArrayList#size.") shouldBe defined
    symtab.resolve("java/lang/String#") shouldBe defined
    //symtab.resolve("java/lang/String#`+`().") shouldBe defined
    symtab.resolve("scala/Predef.ArrowAssoc#`->`().") shouldBe defined
    symtab.resolve("scala/collection/generic/GenericCompanion#apply().") shouldBe defined
  }

}
