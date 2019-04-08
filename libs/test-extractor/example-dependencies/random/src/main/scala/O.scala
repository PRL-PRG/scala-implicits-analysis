import java.io.File
import java.util.UUID
import java.util.UUID.randomUUID

object O extends App {

//import scala.math.Ordered.orderingToOrdered
import scala.math.Ordered

  case class A()
//  case class Loc(file: File, row: Int, col: Int) extends Ordered[Loc] {
//    override def compare(that: Loc): Int = (file, row, col) compare (that.file, that.row, that.col)
//  }
//
//  val l1 = Loc(new File(""), 1, 2)
//  val l2 = Loc(new File(""), 2, 3)
//
//  l1 < l2

  (0,1) < (1,2)
  (A(),0,1) < (A(),1,2)

}

object O2 extends App {
//  trait Ord[A] { def compare(x1: A, x2: A): Int = ??? }
//  implicit class Cmp[A](x: A)(implicit ord: Ord[A]) {
//    def <  (that: A): Boolean = (this compare that) <  0
//  }

  trait =:=[A, B]
  implicit def isEq[A]: A =:= A = new =:=[A, A] {}

  class L[A] {
    def flatten[B](implicit ev: A =:= L[B]): L[B] = ???
    def flatten2[B](implicit ev: A => L[B]): L[B] = ???
  }

  val a = new L[L[Int]]()
  a.flatten2
  a.flatten

  val xs = List(List(1))
  xs.flatten

}