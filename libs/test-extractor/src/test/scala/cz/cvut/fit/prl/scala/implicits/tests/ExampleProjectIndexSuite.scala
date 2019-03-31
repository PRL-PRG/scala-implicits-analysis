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

    ProjectIndex(result.project)
  }

  test("Issue #22 - implicit argument from macro") {
    val module2 = idx.modules.find(_.artifactId == "module2").get
    val css = module2.implicitCallSites
    val testCss = css.filter(_.location.relativeUri.contains("Issue22"))

    testCss should have size 2

    val testCs = testCss.find(_.declarationId.endsWith("test().")).get
    val testCsId = testCs.callSiteId
    testCs.implicitArgumentTypes should contain only CallSiteRef(testCsId - 1)

    val testPosCs = testCss.find(_.callSiteId == testCsId - 1).get
    testPosCs.declarationId shouldBe "org/scalactic/source/Position.apply()."
    testPosCs.parentId shouldBe Some(testCsId)
  }

  test("classpath scope") {
    val module2 = idx.modules.find(_.artifactId == "module2").get
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

    decls.exists(x => x.name == "canBuildFrom" && x.locationScope.contains("compile;managed;dependency;transitive")) should be(
      true)

    decls.exists(x => x.name == "test" && x.locationScope.contains("test;managed;dependency;transitive")) should be(
      true)

    val css = idx.implicitCallSites

    css.exists(x => x.code == "int2string(Example.fun)" && x.locationScope.contains("test")) should be(
      true)
  }
}
