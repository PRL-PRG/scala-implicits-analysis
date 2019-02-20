package module1

import module1.auxiliary.{A, B}

import scala.concurrent.ExecutionContext

object Example {

  implicit var a: A = new A
  implicit val b: B = new B

  implicit val executionContext: ExecutionContext = ExecutionContext.Implicits.global

  implicit def int2string(x: Int): String = x.toString

}
