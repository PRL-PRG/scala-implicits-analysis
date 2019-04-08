import io.circe.generic.auto._
import io.circe.syntax._

case class F(x: Int)
case class B(xs: List[F], ys: List[F])

object App {
  def f: F = F(1)
  def b: B = B(List(F(2)), List(F(3)))

  println(f.asJson, b.asJson)
}
//object Main3 extends App {
//  List(B(List(F("A"), F("B")))).asJson
//}
//
//object Main4 extends App {
//  List(List(B(List(F("A"), F("B"))))).asJson
//}

