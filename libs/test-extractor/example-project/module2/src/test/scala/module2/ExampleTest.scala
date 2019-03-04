package module2

import org.scalatest.{FunSuite, Matchers}

import module1.Example._

class ExampleTest extends FunSuite with Matchers {

  // TODO: move inside the test when #21 is fixed
  implicit val s1: String = Example.fun

  test("one") {
    val s2: String = TestAux.testFun
    s1 should be("1")
    s2 should be("1")
  }

}
