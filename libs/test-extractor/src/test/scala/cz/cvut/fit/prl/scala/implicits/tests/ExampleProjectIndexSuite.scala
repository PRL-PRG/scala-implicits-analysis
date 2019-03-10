package cz.cvut.fit.prl.scala.implicits.tests

import org.scalatest.{FunSuite, Matchers}
import cz.cvut.fit.prl.scala.implicits.model._
import cz.cvut.fit.prl.scala.implicits.tools.ExtractImplicits

class ExampleProjectIndexSuite extends FunSuite with Matchers {

  lazy implicit val idx: Index = {
    val result = ExtractImplicits.extractProject(ExampleProjectPath)

    if (result.exceptions.nonEmpty) {
      result.exceptions.foreach {
        case (_, e) => e.printStackTrace()
      }
    }

    result.exceptions shouldBe empty

    Index(List(result.project))
  }

  test("Issue #22 - implicit argument from macro") {
    val css = idx.modules.values.find(_.artifactId == "module2").get.implicitCallSites
    val testCss = css.filter(_.location.relativeUri.contains("Issue22"))

    testCss should have size 1

    testCss.head.declaration.name should be ("test")
  }

  test("classpath scope") {
    val module2 = idx.modules.filterKeys(_.contains("module2")).head._2
    val paths = module2.paths.values

    paths.exists(x => x.path.contains("semanticdb_2.12") && x.scope == "compile") should be(true)
    paths.exists(x => x.path.contains("scalatest_2.12") && x.scope == "test") should be(true)
    paths.exists(x => x.path.contains("scalacheck_2.12") && x.scope == "test") should be(true)
    paths.exists(x => x.path.contains("rt.jar") && x.scope == "compile") should be(true)
  }

  test("Location scope") {
    val decls = idx.implicitDeclarations

    decls.exists(x => x.name == "int2string" && x.locationScope.contains("compile")) should be(true)

    decls.exists(x => x.name == "testFun" && x.locationScope.contains("test")) should be(true)

    decls.exists(x => x.name == "canBuildFrom" && x.locationScope.contains("compile_dependency")) should be(
      true)

    decls.exists(x => x.name == "test" && x.locationScope.contains("test_dependency")) should be(
      true)

    val css = idx.implicitCallSites

    css.exists(x => x.code == "int2string(Example.fun)" && x.locationScope.contains("test")) should be(
      true)
  }
}
