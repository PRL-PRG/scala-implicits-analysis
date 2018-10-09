package cz.cvut.fit.prl.scala.implicits

import org.scalatest.Matchers

class SymtabSuite extends SemanticdbSuite with Matchers {

  test("symbol resolution") {
    symtab.info("java/util/ArrayList#size.") shouldBe defined
    symtab.info("java/lang/String#") shouldBe defined
    //symtab.info("java/lang/String#`+`().") shouldBe defined
    symtab.info("scala/Predef.ArrowAssoc#`->`().") shouldBe defined
    symtab.info("scala/collection/generic/GenericCompanion#apply().") shouldBe defined
  }

}
