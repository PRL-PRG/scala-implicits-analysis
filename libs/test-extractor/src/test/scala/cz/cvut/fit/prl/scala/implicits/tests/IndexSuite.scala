package cz.cvut.fit.prl.scala.implicits.tests

import org.scalatest.{FunSuite, Matchers}
import cz.cvut.fit.prl.scala.implicits.model.Index
import cz.cvut.fit.prl.scala.implicits.tools.ExtractImplicits

class IndexSuite extends FunSuite with Matchers {

  lazy val idx: Index = {
    val result = ExtractImplicits.extractProject(ProjectPath)

    result.exceptions shouldBe empty

    Index(List(result.project))
  }

  test("Index") {
    idx.implicitDeclarations.size shouldBe 17
    idx.implicitCallSites.size shouldBe 11
  }
}
