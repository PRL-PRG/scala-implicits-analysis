package com.example

import scala.concurrent.ExecutionContext
import scala.language.implicitConversions
import com.example.auxiliary._

object Example {

  implicit var a: A = new A
  implicit val b: B = new B

  implicit val executionContext: ExecutionContext = ExecutionContext.Implicits.global

  implicit def int2string(x: Int): String = x.toString

}
