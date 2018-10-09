package cz.cvut.fit.prl.scala.implicits

import org.scalatest.Matchers

import scala.meta.internal.{semanticdb => s}
import scala.reflect.ClassTag

class CallSiteExtractorSuite extends SemanticdbSuite with Matchers {

  implicit class XtensionOption[T](x: Option[T]) {
    def check[U <: T : ClassTag](tester: U => Unit) = {
      x match {
        case Some(y) => y.check[U](tester)
        case None => throw new Exception("No such element")
      }
    }
  }

  implicit class XtensionAny[T](x: T) {
    def check[U <: T : ClassTag](tester: U => Unit): Unit = {
      val clazz = implicitly[ClassTag[U]].runtimeClass
      if (!clazz.isAssignableFrom(x.getClass)) {
        fail(s"$x is not instanceof $clazz")
      }
      tester(x.asInstanceOf[U])
    }
  }

  def checkNoFailures(extractor: CallSiteExtractor): Unit = {
    extractor.failures.foreach(_.printStackTrace())
    extractor.failures shouldBe empty
  }

  // TC
  //  database(
  //    """
  //      |object X {
  //      |  trait Jsonable[-T] {
  //      |    def toJson(x: T): String
  //      |  }
  //      |
  //      |  implicit val int2jsonable: Jsonable[Int] =  { x: Int => x.toString }
  //      |
  //      |  implicit def traversable2jsonable[T: Jsonable] = new Jsonable[Traversable[T]] {
  //      |    override def toJson(x: Traversable[T]): String = {
  //      |      val tc = implicitly[Jsonable[T]]
  //      |      x.map(tc.toJson).mkString("[", ",", "]")
  //      |    }
  //      |  }
  //      |
  //      |  implicit class XtensionJson[T: Jsonable](x: T) {
  //      |    def toJson: String = implicitly[Jsonable[T]].toJson(x)
  //      |  }
  //      |
  //      |  //def json[T](x: T)(implicit e: Jsonable[T]): String = {
  //      |  //  e.toJson(x)
  //      |  //}
  //      |
  //      |  Seq(1,2,3).toJson
  //      |  //json(Seq(1,2,3))
  //      |}
  //    """.stripMargin
  //  ) { db =>
  //  }
  //

  //  database(
  //    """
  //      |object X {
  //      |  Seq(1) ++ Seq(2)
  //      |}
  //    """.stripMargin) { db =>
  //    val cs = CallSite.extractCallSites(db.text.parse[Source].get)
  //    println(cs.mkString("\n\n"))
  //    println(db.synthetics.map(x => x -> new LegacySyntheticPrinter().toLegacy(x)).mkString("\n\n"))
  //    println()
  //  }

  //    println(cs.mkString("\n\n"))
  //    println(synthetics.map(x => x -> new LegacySyntheticPrinter().toLegacy(x)).mkString("\n\n"))
  //    println()

  def extraction(code: String)(fn: CallSiteExtractor => Unit): Unit = {
    implicit object RangeSorted extends Ordering[s.Range] {
      override def compare(x: s.Range, y: s.Range): Int = {
        val d = x.startLine.compare(y.startLine)
        if (d == 0) x.startCharacter.compare(y.startCharacter) else d
      }
    }

    database(code)(db => {
      val extractor = new CallSiteExtractor(db, symtab)
      checkNoFailures(extractor)
      fn(extractor)
    })
  }

  extraction(
    """
      | object NestedCall {
      |   math.max(1+2, 3+4)
      | }
    """.stripMargin) { extractor =>
    val css = extractor.callSites
    css should have size 3
  }

  extraction(
    """
      |object InferredApplyCall {
      |  Seq(1)
      |}
    """.stripMargin) { extractor =>
    extractor.failures shouldBe empty

    val css = extractor.callSites
    css should have size 1

    css.find(_.declaration.name == "apply") check[NormalCall] { x =>
      x.typeArgs should have size 1
    }
  }

  //  extraction(
  //    """
  //      |
  //    """.stripMargin) { extractor =>
  //
  ////    class A
  ////    class B
  ////    implicit def a2b(x: A): B = new B
  ////    implicit val a = new A
  ////    def f(x: Int)(implicit b: B) = 1
  ////
  ////    f(1)
  //  }

  extraction(
    """
      |
    """.stripMargin
  ) { extraction =>
    extraction.callSites shouldBe empty
  }

  extraction(
    """
      |object NestedCallsInParams {
      |  def a(x: Int) = 1
      |  def b(x: Int) = 2
      |  def c(x: Int) = 3
      |  def f(x: Int, y: Int) = 5
      |  object d {
      |    def e(x: Int) = 6
      |  }
      |
      |  f(a(b(c(4))), d.e(c(7)))
      |}
      |
    """.stripMargin
  ) { extraction =>
    extraction.callSites should have size 6
  }

  extraction(
    """
      | object NonLocalImplicitParameter {
      |   Seq(1) ++ List(1)
      | }
    """.stripMargin) { extractor =>

    val css = extractor.callSites
    css should have size 4

    css.find(_.declaration.name == "canBuildFrom") check[SyntheticCall] { x =>
      x.typeArgs should have size 1
      x.typeArgs.head.check[TypeRef] { x =>
        x.symbol.displayName shouldBe "Int"
      }
    }
    css.find(_.code == "List(1)") check[NormalCall] { x =>
      x.typeArgs should have size 1
    }
    css.find(_.code == "Seq(1)") check[NormalCall] { x =>
      x.typeArgs should have size 1
    }
    css.find(_.declaration.name == "++") check[NormalCall] { x =>
      x.implicitArgs.check[ArgumentsList] { x =>
        x.args should have size 1
        x.args.head.check[CallSiteRef] { x =>
          x.callSite shouldBe css.find(_.declaration.name == "canBuildFrom").get
        }
      }
      x.typeArgs should have size 2
    }
  }

  // TODO:  "object X { val x: a.b = c.d }"
  // TODO:  type +[A,B] = Tupel2[A,B]; val x: A+B = 1

//  extraction(
//    """
//      |object X {
//      |  def f[T](x: T) = x
//      |  f(1)
//      |}
//    """.stripMargin) { extractor =>
//
//    val css = extractor.callSites
//    css should have size 1
//
//    css.find(_.declaration.name == "f") check[NormalCall] { x =>
//      x.typeArgs should have size 1
//    }
//
//    import scala.math.Ordered.orderingToOrdered
//
//    val x: java.io.File = ???
//    val y = 42
//    (x, y)
//      .compare((x, y))
//
//  }


  extraction(
    """
      |object X {
      |  object A {
      |    def apply(x: Int) = x
      |  }
      |  A(1)
      |}
    """.stripMargin) { extractor =>

    val css = extractor.callSites
    css should have size 1

    css.find(_.declaration.name == "apply") check[NormalCall] { x =>
      x.argss should have size 1
      x.argss.head.args should have size 1
    }
  }

  extraction(
    """
      |object X {
      |  object A {
      |    def apply(x: String)(implicit y: Int) = x
      |  }
      |  implicit val iy = 1
      |  A("A")
      |}
    """.stripMargin) { extractor =>

    val css = extractor.callSites
    css should have size 1

    css.find(_.declaration.name == "apply") check[NormalCall] { x =>
      x.implicitArgs.check[ArgumentsList] { list =>
        list.syntactic shouldBe false
        list.args should have size 1
      }
    }
  }

  extraction(
    """
      |object X {
      |  object A {
      |    def apply[T](x: T)(implicit y: T) = x
      |  }
      |  implicit val iy = 1
      |  A(1)
      |}
    """.stripMargin) { extractor =>
    extractor.failures shouldBe empty

    val css = extractor.callSites
    css should have size 1

    css.find(_.declaration.name == "apply") check[NormalCall] { x =>
      x.implicitArgs.check[ArgumentsList] { list =>
        list.syntactic shouldBe false
        list.args should have size 1
      }
      x.typeArgs should have size 1
    }
  }

  extraction(
    """
      |object X1 {
      |  object A {
      |    def apply[T](x: T)(implicit y: T) = x
      |  }
      |  implicit val iy = 1
      |  A.apply[Int](1)
      |}
    """.stripMargin) { extractor =>

    val css = extractor.callSites
    css should have size 1

    css.find(_.declaration.name == "apply") check[NormalCall] { x =>
      x.implicitArgs.check[ArgumentsList] { list =>
        list.syntactic shouldBe false
        list.args should have size 1
      }
      x.typeArgs should have size 1
    }
  }

  extraction(
    """
      |object X2 {
      |  object A {
      |    def apply[T](x: T)(implicit y: T) = x
      |  }
      |  implicit val iy = 1
      |  A.apply[Int](1)(2)
      |}
    """.stripMargin) { extractor =>

    val css = extractor.callSites
    css should have size 1

    css.find(_.declaration.name == "apply") check[NormalCall] { x =>
      x.implicitArgs.check[ArgumentsList] { list =>
        list.syntactic shouldBe true
        list.args should have size 1
      }
      x.typeArgs should have size 1
    }
  }

  extraction(
    """
      |object X2 {
      |  class A(x: Int)(y: String)(implicit z: Boolean)
      |  implicit val iz = true
      |  new A(1)("A")
      |}
    """.stripMargin) { extractor =>

    val css = extractor.callSites
    css should have size 1

    css.find(_.declaration.name == "<init>") check[NormalCall] { x =>
      x.implicitArgs.check[ArgumentsList] { list =>
        list.syntactic shouldBe false
        list.args should have size 1
      }
      x.typeArgs should have size 0
    }
  }

  extraction(
    """
      |object ClassConstructorImplicitTypeArgs {
      |  class A[T](x: T)(y: String)(implicit z: Boolean)
      |  implicit val iz = true
      |  new A(1)("A")
      |}
    """.stripMargin) { extractor =>

    val css = extractor.callSites
    css should have size 1

    css.find(_.declaration.name == "<init>") check[NormalCall] { x =>
      x.implicitArgs.check[ArgumentsList] { list =>
        list.syntactic shouldBe false
        list.args should have size 1
      }
      // there are no type arguments, only parameters
      x.typeArgs should have size 0
    }
  }

  //  class X
  //  class X2 extends X

  extraction(
    """
      |object ClassConstructorExplicitImplicitTypeArgs {
      |  class A[T](x: T)(y: String)(implicit z: Boolean)
      |  implicit val iz = true
      |  new A[Any](1)("A")(false)
      |}
    """.stripMargin) { extractor =>
    extractor.failures shouldBe empty

    val css = extractor.callSites
    css should have size 1

    css.find(_.declaration.name == "<init>") check[NormalCall] { x =>
      x.implicitArgs.check[ArgumentsList] { list =>
        list.syntactic shouldBe true
        list.args should have size 1
      }
      x.typeArgs should have size 1
    }
  }

  extraction(
    """
      | object ExpressionsArguments {
      |   def f(x: Int) = x
      |   f(1+1)
      | }
    """.stripMargin
  ) { extractor =>
    val css = extractor.callSites
    css should have size 2

  }

  extraction(
    """
      | object BasicStringInterpolation {
      |   val z = "A"
      |   s"Test $z"
      | }
    """.stripMargin
  ) { extractor =>
    val css = extractor.callSites
    css should have size 1

  }

  // FIXME: making z a string does not work - "java/lang/String#`+`()." cannot be resolved in symtab?!
  extraction(
    """
      | object StringInterpolationWithExpression {
      |   val z = 1 // "A"
      |   s"Test ${z + 1}"
      | }
    """.stripMargin
  ) { extractor =>
    val css = extractor.callSites
    css should have size 2
  }

  extraction(
    """
      | object StringInterpolationWithMap {
      |   val xs = Seq(1)
      |   s"Test ${xs.map(_ + 1)}"
      | }
    """.stripMargin
  ) { extractor =>
    val css = extractor.callSites
    css should have size 5
  }

  extraction(
    """
      |package a.b.c
      |
      |object SelectInPackage {
      |}
    """.stripMargin
  ) { extractor => extractor.callSites shouldBe empty }

  extraction(
    """
      |import scala.concurrent.Future
      |
      |object SelectInImport {
      |}
    """.stripMargin
  ) { extractor => extractor.callSites shouldBe empty }

  extraction(
    """
      |object SelectInVar {
      |  var x: scala.collection.Seq[Int] = null
      |}
    """.stripMargin) { extractor =>
    extractor.callSites shouldBe empty
  }

  extraction(
    """
      |object SelectInVal {
      |  val x: scala.collection.Seq[Int] = null
      |}
    """.stripMargin) { extractor =>
    extractor.callSites shouldBe empty
  }

  extraction(
    """
      |object SelectInParam {
      |  def f(x: scala.collection.Seq[Int]): scala.collection.Seq[Int] = x
      |}
    """.stripMargin) { extractor =>
    extractor.callSites shouldBe empty
  }

  extraction(
    """
      |object SelectAsCall {
      |  "A".hashCode.hashCode
      |}
    """.stripMargin) { extractor =>
    val css = extractor.callSites
    css should have size 2
  }

//    extraction(
//      """
//        |object CompareOnTuple {
//        |  import scala.math.Ordered.orderingToOrdered
//        |
//        |  val f: java.io.File = ???
//        |  val i: Int = 1
//        |  val x =
//        |    (f, i)
//        |      .compare((f, i))
//        |}
//      """.stripMargin
//    ) { extractor =>
//      val css = extractor.callSites
//      css should have size 4
//    }

  // TODO: new A[]
  // TODO: new A[](impl)
  // TODO: new A[] {}
  // TODO: new A[](impl) {}
  // TODO: a.+[A](impl)
  // TODO: -[A]x(impl)
  // TODO: multiple parameter lists

  //
  //  extraction(
  //    """
  //      |object X {
  //      |  val List(a,b) = Seq(1,2)
  //      |}
  //    """.stripMargin) { extractor =>
  //
  //    val css = extractor.callSites
  //    css should have size 3
  //
  //    // TOD0: there should be two apply
  //    css.find(_.fun.name == "apply") check { x =>
  //      x shouldBe a[NormalCall]
  //    }
  //    css.find(_.fun.name == "unapplySeq") check { x =>
  //      x shouldBe a[SyntheticCall]
  //    }
  //  }
  //
  //  extraction(
  //    """
  //      |object X {
  //      |  1 -> 2
  //      |}
  //    """.stripMargin) { extractor =>
  //    extractor.failues shouldBe empty
  //
  //    val css = extractor.callSites
  //    css should have size 2
  //
  //    css.find(_.fun.name == "ArrowAssoc") check { x =>
  //      x shouldBe a[ConversionCall]
  //    }
  //    css.find(_.fun.name == "->") check { x =>
  //      x shouldBe a[NormalCall]
  //    }
  //  }
  //
  //  extraction(
  //    """
  //      |object X {
  //      |  "hi".stripMargin
  //      |}
  //    """.stripMargin) { extractor =>
  //    extractor.failues shouldBe empty
  //
  //    val css = extractor.callSites
  //    css should have size 2
  //
  //    css.find(_.fun.name == "stripMargin") check { x =>
  //      x shouldBe a[NormalCall]
  //    }
  //    css.find(_.fun.name == "augmentString") check { x =>
  //      x shouldBe a[ConversionCall]
  //    }
  //  }

  //  extraction(
  //    """
  //      |object X {
  //      |  List(1).map(_ + 2)
  //      |}
  //    """.stripMargin) { extractor =>
  //    extractor.failues shouldBe empty
  //
  //    val css = extractor.callSites
  //    println(css)
  //
  //    css should have size 3
  //
  //  }

  extraction(
    """
      |object SimpleFor {
      |  for {
      |    i <- Seq(1,2)
      |  } yield i.toString
      |}
    """.stripMargin) { extractor =>

    val css = extractor.callSites

    css should have size 4

    css.find(_.declaration.name == "apply") check[NormalCall] { x =>
    }
    css.find(_.declaration.name == "toString") check[NormalCall] { x =>
    }
    css.find(_.declaration.name == "map") check[SyntheticCall] { x =>
      x.argss should have size 2
      x.implicitArgs.check[ArgumentsList] { x =>
        x.syntactic shouldBe false
        x.args should have size 1
        x.args.head.check[CallSiteRef] { x =>
          x.callSite.declaration.name shouldBe "canBuildFrom"
        }
      }
    }
  }

  //    Seq(1,2) map (i=> i + 1)

  extraction(
    """
      |object NestForWithFilter {
      |  for {
      |    i <- 1 to 10 if i % 2 == 0
      |    j <- Seq('A', 'B')
      |  } yield (i, j)
      |}
    """.stripMargin) { extractor =>

    val css = extractor.callSites

    css should have size 10
  }

  extraction(
    """
      |object ForLoopWithFuture {
      |  import scala.concurrent.Future
      |  import scala.concurrent.ExecutionContext.Implicits.global
      |
      |  for {
      |    a <- Future.successful(1)
      |    b <- Future.successful(2)
      |  } a + b
      |}
    """.stripMargin) { extractor =>
    val css = extractor.callSites

    css should have size 5
  }

  // TODO:
  //     (1 to 10).withFilter(i => i % 2 == 0).flatMap(i =>
  //      Seq('A','B').map(j => (i,j))
  //    )

  // TODO: test on explicit nested calls like f(a[A](b[B#C](c.d[C])), d(e(),f(g())))

  //
  //  extraction(
  //    """
  //      |object X {
  //      |  List(1)
  //      |}
  //    """.stripMargin) { extractor =>
  //    extractor.failues shouldBe empty
  //
  //    val css = extractor.callSites
  //    css should have size (1)
  //
  //    val cs = css.head
  //    cs shouldBe a[NormalCall]
  //    cs.fun shouldBe "List"
  //  }
  //
  //  extraction(
  //    """
  //      |object X {
  //      |  val List(a,b) = Seq(1,2)
  //      |}
  //    """.stripMargin) { extractor =>
  //    extractor.failues shouldBe empty
  //
  //    val css = extractor.callSites
  //    css should have size (1)
  //
  //    val cs = css.head
  //    cs shouldBe a[NormalCall]
  //    cs.fun shouldBe "Seq"
  //  }

  // "fooo".stripPrefix("o")
  // 1 #:: 2 #:: Stream.empty
  // Array.empty[Int]
  // for {
  //  i <- 1 to 10
  //  j <- 0 until 10
  //  if i % 2 == 0
  //} yield (i, j)
  // for {
  //   a <- Future.successful(1)
  //   b <- Future.successful(2)
  // } println(a)

  //  database(
  //    """
  //      | object X {
  //      |   val a = 1
  //      |   s"$a + 1"
  //      |   s"${a + 1}"
  //      |   s"${a.hashCode}"
  //      | }
  //    """.stripMargin) { db =>
  //
  //    val tree = db.text.parse[Source].get
  //    println(CallSite.extractCallSites(tree).mkString("\n\n"))
  //  }

  //  database(
  //    """
  //      | object X {
  //      |   val x = new StringBuffer
  //      |   val y = new StringBuffer()
  //      |   val z = new Runnable() {
  //      |   }
  //      | }
  //    """.stripMargin) { db =>
  //
  //    val tree = db.text.parse[Source].get
  //    println(CallSite.extractCallSites(tree).mkString("\n\n"))
  //  }

  //  database(
  //    """
  //      |object X {
  //      |  //Seq(1).map(_+1).mkString(",")
  //      |  ("A"+"B").getBytes().map(_+1).mkString(",").length.hashCode
  //      |  Seq(1).size.toString()
  //      |}
  //    """.stripMargin
  //  ) { db =>
  //
  //    val tree = db.text.parse[Source].get
  //    println(CallSite.extractCallSites(tree).mkString("\n\n"))
  //    //println(db.synthetics.map(x => x -> new LegacySyntheticPrinter().toLegacy(x)).mkString("\n\n"))
  //  }

  //  database(
  //    """
  //      |object X {
  //      |  Seq(1,2,3)
  //      |}
  //    """.stripMargin
  //  ) { db =>
  //    print(db.synthetics)
  //  }

//  extraction(
//    """
//      |object X {
//      |  trait Jsonable[-T] {
//      |    def toJson(x: T): String
//      |  }
//      |
//      |  implicit val int2jsonable: Jsonable[Int] =  { x: Int => x.toString }
//      |
//      |  implicit def traversable2jsonable[T: Jsonable] = new Jsonable[Traversable[T]] {
//      |    override def toJson(x: Traversable[T]): String = {
//      |      val tc = implicitly[Jsonable[T]]
//      |      x.map(tc.toJson).mkString("[", ",", "]")
//      |    }
//      |  }
//      |
//      |  implicit class XtensionJson[T: Jsonable](x: T) {
//      |    def toJson: String = implicitly[Jsonable[T]].toJson(x)
//      |  }
//      |
//      |  //def json[T](x: T)(implicit e: Jsonable[T]): String = {
//      |  //  e.toJson(x)
//      |  //}
//      |
//      |  Seq(1,2,3).toJson
//      |  //json(Seq(1,2,3))
//      |}
//    """.stripMargin) { extractor =>
//      val css = extractor.callSites
//    }
}
