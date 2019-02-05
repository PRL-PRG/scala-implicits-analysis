package com.example

object JsonExample extends App {

  def json[T](x: T)(implicit e: Jsonable[T]): String = {
    e.toJson(x)
  }

  implicit val int2jsonable: Jsonable[Int] = { x: Int =>
    x.toString
  }

  implicit def traversable2jsonable[T: Jsonable]: Jsonable[Traversable[T]] =
    new Jsonable[Traversable[T]] {
      override def toJson(x: Traversable[T]): String = {
        val tc = implicitly[Jsonable[T]]
        x.map(tc.toJson).mkString("[", ",", "]")
      }
    }

  implicit class XtensionJson[T: Jsonable](x: T) {
    def toJson: String = implicitly[Jsonable[T]].toJson(x)
  }

  trait Jsonable[-T] {
    def toJson(x: T): String
  }

  println(Seq(1, 2, 3).toJson)
  println(json(Seq(1, 2, 3)))

}
