package module2

import org.scalactic.source.Position
import org.scalatest.FunSuite

class Issue22 extends FunSuite {

  test("test") {

  }

  test("test2") {

  }(Position("a", "b", 1))

}
