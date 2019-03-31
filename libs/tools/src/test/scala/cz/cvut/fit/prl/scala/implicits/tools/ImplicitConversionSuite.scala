package cz.cvut.fit.prl.scala.implicits.tools

import cz.cvut.fit.prl.scala.implicits.model._
import cz.cvut.fit.prl.scala.implicits.extractor.{ExtractionContextSuite, ModelSimplification}
import cz.cvut.fit.prl.scala.implicits.model.Language.JAVA

import scala.util.Success

class ImplicitConversionSuite
    extends ExtractionContextSuite
    with ModelSimplification
    with ModelDSL {

  import ModelDSL._
  import ImplicitConversionExporter.export

  project("conversion export of json example",
    """
      | package p
      | object o {
      |   trait Jsonable[T] {
      |     def toJson(x: T): String
      |   }
      |
      |   implicit val intJsonable = new Jsonable[Int] {
      |     def toJson(x: Int): String = x.toString
      |   }
      |
      |   implicit def seqJsonable[T: Jsonable]: Jsonable[Seq[T]] = new Jsonable[Seq[T]] {
      |     def toJson(xs: Seq[T]): String = {
      |       xs.map(_.toJson).mkString(",")
      |     }
      |   }
      |
      |   implicit class XtensionJson[T: Jsonable](x: T) {
      |     def toJson: String = implicitly[Jsonable[T]].toJson(x)
      |   }
      |
      |   Seq(1,2,3).toJson
      | }
    """.stripMargin) { implicit project =>
      val conversion = export(project)
      val declarations = project.implicitDeclarations.toList
      val callSites = project.implicitCallSites.toList

      println()
  }

  index(
    "simple type parents",
    """
      | package p
      | object o {
      |   class Person
      |
      |   implicit val v = List(new Person)
      | }
    """.stripMargin) { implicit idx =>
    implicit val m = idx.modules.head

    val v = idx.resolveDeclaration(TestModuleId, "p/o.v.")

    val fun = v.returnType.get.allParents.dropWhile(x => !x.isTypeOf(declarationIds.Function1))

    fun.head.typeArguments should contain inOrderOnly (
      typeRef("scala/Int#"),
      typeRef("p/o.Person#")
    )
  }

  index(
    "declaration parameters",
    """
      | package p
      | object o {
      |   class Person[T1, T2]
      |
      |   type Nested = Int
      |   type Nested2 = Nested
      |   type Top[X] = Person[Nested2, X]
      |   type Top2 = Top[Boolean]
      |   type C[X] = List[X]
      |   type C2[X] = C[X]
      |
      |   implicit val v: C2[Top2] = List(new Person[Int, Boolean])
      | }
    """.stripMargin) { implicit idx =>
    implicit val m = idx.modules.head

    val v = idx.resolveDeclaration(TestModuleId, "p/o.v.")
    val fun = v.returnType.get.allParents.dropWhile(x => !x.isTypeOf(declarationIds.Function1))

    val Seq(from, to) = fun.head.typeArguments

    from shouldBe typeRef("scala/Int#")
    to shouldBe typeRef("p/o.Top2#")
    to.resolveType shouldBe typeRef(
      "p/o.Person#",
      typeRef("p/o.Nested2#"),
      typeRef("scala/Boolean#"))
    to.resolveFullType shouldBe typeRef(
      "p/o.Person#",
      typeRef("scala/Int#"),
      typeRef("scala/Boolean#")
    )
  }

  // akka-actor/src/main/scala/akka/util/Helpers.scala
  index(
    "requiring",
    """
      | package p
      | object o {
      |   implicit class Requiring[A](val value: A) extends AnyVal {
      |     def requiring(cond: A ⇒ Boolean, msg: ⇒ Any): A = {
      |       require(cond(value), msg)
      |       value
      |     }
      |   }
      | }
    """.stripMargin) { implicit idx =>
    val requiring = idx.resolveDeclaration(TestModuleId, "p/o.Requiring().")
    val exported = export(requiring).get.get

    exported.from shouldBe typeRef("p/o.Requiring().[A]")
    exported.to shouldBe typeRef("p/o.Requiring#", typeRef("p/o.Requiring().[A]"))
  }

  index(
    "requiring2",
    """
      | package p
      | object o {
      |   class A
      |   class B extends A
      |   class C extends B
      |
      |   class Pair[T1,T2]
      |
      |   type U = B
      |   type IntPair[X] = Pair[Int, X]
      |
      |   implicit def f[T1 <: IntPair[_ <: U]](x: T1) = true
      |   implicit def g[T1 <: IntPair[U]](x: T1) = 1
      |   val p = new Pair[Int, C]()
      |
      |   if (f(p)) println(1)
      | }
    """.stripMargin) { implicit idx =>
    implicit val m = idx.modules.head

    val f = idx.resolveDeclaration(TestModuleId, "p/o.f().")
    val g = idx.resolveDeclaration(TestModuleId, "p/o.g().")
    val ef = export(f).get.get
    val eg = export(g).get.get

    eg.from.resolveFullType shouldBe typeRef("p/o.Pair#", typeRef("scala/Int#"), typeRef("p/o.B#"))

    // we do not support existentials so the following still keeps the reference
    ef.from.resolveFullType shouldBe typeRef(
      "p/o.Pair#",
      typeRef("scala/Int#"),
      typeRef("p/o.IntPair#[X]"))
  }

  // http://scalapuzzlers.com/#pzzlr-054
  project(
    "pzzlr-054",
    """
      | package p
      | object o {
      |   case class Card(number: Int, suit: String = "clubs") {
      |     def isInDeck(implicit deck: List[Card]) = deck contains this
      |   }
      |
      |   implicit val deck = List(Card(1, "clubs"))
      |   implicit def intToCard(n: Int) = Card(n)
      |
      |   println(1.isInDeck)
      | }
    """.stripMargin) { project =>
    val exported = export(project).toList.collect { case Success(v) => v }
    val names = exported.map(_.declaration.name)

    names should contain only ("deck", "intToCard")
  }

  index(
    "conversion local to a block",
    """
      | package p
      | object o {
      |   def x: Unit = {
      |     implicit def i2s(x: Int): String = "A"
      |     val s: String = 1
      |   }
      | }
    """.stripMargin) { idx =>
    val i2s = idx.implicitDeclarations.head
    i2s.isBlockLocal shouldBe true
  }

  // https://github.com/lampepfl/dotty/pull/2060#issuecomment-284798490
  index(
    "same compilation unit",
    """
      | package p1 {
      |   class Foo[T](t: T)
      | }
      |
      | package p2 {
      |   object Cake {
      |     type Foo = p1.Foo[String]
      |     implicit def makeFoo(s: String) = new Foo(s)
      |   }
      | }
      |
      | package p3 {
      |   object Cake {
      |     class Foo(s: String) extends p1.Foo[String](s)
      |     implicit def makeFoo(s: String) = new Foo(s)
      |   }
      | }
      |
      | package p4 {
      |   class Foo[T](t: T)
      |
      |   trait FooImplicits[T] {
      |     implicit def makeFoo(s: T): Foo[T] = realMakeFoo(s)
      |     def realMakeFoo(s: T): Foo[T]
      |   }
      |
      |   object Cake extends FooImplicits[String] {
      |     type Foo = p4.Foo[String]
      |     def realMakeFoo(s: String) = new Foo(s)
      |   }
      | }
    """.stripMargin) { idx =>
    implicit val m = idx.modules.head

    val p2makeFoo = m.resolveDeclaration( "p2/Cake.makeFoo().")
    val p3makeFoo = m.resolveDeclaration( "p3/Cake.makeFoo().")
    val p4makeFoo = m.resolveDeclaration( "p4/FooImplicits#makeFoo().")

    p2makeFoo.compilationUnit should not be p2makeFoo.returnType.get.declaration.compilationUnit
    p3makeFoo.compilationUnit shouldBe p3makeFoo.returnType.get.declaration.compilationUnit
    p4makeFoo.compilationUnit should not be p4makeFoo.returnType.get.declaration.compilationUnit
  }

  project(
    "conversions using def",
    """
      |package p
      |object o {
      |  implicit def g1(x: Int): String = ""
      |  implicit def g2(x: Int)(implicit y: Int): String = ""
      |  implicit def g3(x: Int)(implicit y: Int, z: Int): String = ""
      |
      |  implicit def n1(x: Int): Unit = ()
      |  implicit def n2(x1: Int)(x2: Int): Unit = ()
      |  implicit def n3(x1: Int, x2: Int)(implicit y: Int): String = ""
      |  implicit def n4(x1: Int)(x2: Int)(implicit y: Int): String = ""
      |}
    """.stripMargin) { project =>
    val exported = export(project).collect {
      case Success(x) => x
    }.toList

    // n1 has unit return type
    // n2,n3,n4 have more than one parameter
    exported.map(_.declaration.name) should contain only ("g1", "g2", "g3")
  }

  project(
    "conversions using class",
    """
      |package p
      |object o {
      |  implicit class G1(x: Int) {
      |    def x = 1
      |  }
      |  implicit class G2(x: Int)(implicit y: Int) {
      |    def x = 1
      |  }
      |}
    """.stripMargin) { project =>
    val exported = export(project).collect {
      case Success(x) => x
    }.toList

    // we do not need to worry about number of arguments as the scalac will do that
    // we use declarationId since just name is ambiguous and G1 could also mean class G1
    exported.map(_.declaration.declarationId) should contain only ("p/o.G1().", "p/o.G2().")
  }

  project(
    "conversions using val",
    """
      |package p
      |object o {
      |  implicit val g1 = (x: Int) => x.toString
      |
      |  implicit val n1 = (x: Int, y: Int) => x.toString
      |  implicit val n2 = (x: Int) => ()
      |}
    """.stripMargin) { project =>
    val exported = export(project).collect {
      case Success(x) => x
    }.toList

    // n1 have too many parameters
    // n2 has return type scala.Unit
    exported.map(_.declaration.name) should contain only "g1"
  }

  project(
    "conversions with type",
    """
      |package p
      |object o {
      |  import scala.language.implicitConversions
      |  type A = Int
      |  type B = A
      |  type C = Unit
      |
      |  implicit def g(x: Int): B = 1
      |  implicit def n(x: Int): C = ()
      |}
    """.stripMargin) { project =>
    val exported = export(project).collect {
      case Success(x) => x
    }.toList

    // n returns C which is Unit
    exported.map(_.declaration.name) should contain only "g"
  }

  index(
    "extension interface",
    """
      |package p
      |object o {
      |  implicit class Xtension(x: Int) extends Runnable {
      |    def run(): Unit = {}
      |  }
      |}
      """.stripMargin) { idx =>
    implicit val m = idx.modules.head

    val xtension = m.resolveDeclaration("p/o.Xtension#")
    xtension.parents.head.declaration.language should be(JAVA)
  }

//  implicits(
//    "extension methods",
//    """
//      |package p
//      |object o {
//      |  implicit val u: Unit = ()
//      |  implicit val i: Int = 1
//      |  implicit val j: Boolean = true
//      |  implicit val k: String = ""
//      |
//      |  implicit def f(x: Int): Unit = ()
//      |  implicit def g(x: Int)(implicit y: Int): String = ""
//      |  implicit val gv: Int => String = x => ""
//      |  implicit object go extends (Int => String) {
//      |    override def apply(v1: Int): String = ""
//      |  }
//      |
//      |  implicit class Xtension(x: Int) {
//      |    def x = "A"
//      |  }
//      |  1.x
//      |}
//    """.stripMargin) { (ds, cs) =>
//    }
//
//  declarations("implicit conversion as def",
//    """
//      |package p
//      |object o {
//      |  implicit def i2s(x: Int): String = x.toString
//      |  val s: String = 1
//      |}
//    """.stripMargin) { implicit res =>
// println()
//  }
//
//  declarations("implicit conversion as val",
//    """
//      |package p
//      |object o {
//      |  implicit val i2s: Int => String = x => x.toString
//      |  val s: String = 1
//      |}
//    """.stripMargin) { implicit res =>
// println()
//  }

  callSites(
    "companion object",
    """
      |package p
      |object o {
      |
      |  class URL
      |
      |  object URL {
      |    implicit def apply(s: String): URL = new URL
      |  }
      |
      |  def download(url: URL): Unit = {}
      |
      |  download("http://fit.cvut.cz")
      |
      |}
    """.stripMargin) { implicit res =>
    // _empty_/o.URL.
    res.callSites.head.declarationId should be("p/o.URL.apply().")
  }

//  callSites("nested conversions3",
//    """
//      |object o {
//      |
//      |  class A
//      |  class B
//      |  class C
//      |
//      |  implicit def a2X(implicit a: A): B = new B
//      |  implicit def a2X(implicit b: B): C = new C
//      |
//      |  implicit val va: A = new A
//      |
//      |  def f(implicit b: B, c: C) = 1
//      |
//      |  f // f(a2X(va), a2X(a2X(va)))
//      |
//      |}
//    """.stripMargin) { implicit res =>
//    res.callSites.apply(1)
//  }

//  callSites("nested conversions2",
//    """
//      |object o {
//      |
//      |  trait T
//      |  class A
//      |  trait B extends T
//      |  trait C extends T
//      |
//      |  implicit def a2t[X <: T](implicit a: A): X = 1.asInstanceOf[X]
//      |
//      |  implicit val va: A = new A
//      |
//      |  def f(implicit b: B, c: C) = 1
//      |
//      |  f
//      |
//      |}
//    """.stripMargin) { implicit res =>
//    res.callSites.apply(1)
//  }

//  callSites(
//    "implicit conversion in implicit parameters",
//    """
//      |
//    """.stripMargin) { res =>
//    }
//
////  3. identify implicit conversion with implicit parameters
////  4. identify implicit conversion with implicit parameters in implicit parameters
////  5. identify if the conversion is coming from a companion object
//
//  project(
//    "implicit conversion chains",
//    """
//      |object o {
//      |
//      |  class A
//      |  class B
//      |  class C
//      |
//      |  implicit def a2b(implicit a: A): B = new B
//      |  implicit def b2c(implicit b: B): C = new C
//      |
//      |  implicit val va: A = new A
//      |
//      |  def f(implicit c: C) = 1
//      |
//      |  f
//      |
//      |}
//    """.stripMargin) { project =>
//    // TODO: should these be considered?
//  }
//
//  // TODO: implicit conversion with implicit parameters
}
