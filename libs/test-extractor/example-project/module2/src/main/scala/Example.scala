import com.example.JsonExample._

object Example extends App {

  implicit val string2jsonable: Jsonable[String] = { x: String =>
    s""""$x""""
  }

  println(Seq("A", "B").toJson)

}
