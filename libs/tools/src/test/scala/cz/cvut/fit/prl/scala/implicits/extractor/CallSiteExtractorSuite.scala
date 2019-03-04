package cz.cvut.fit.prl.scala.implicits.extractor

import cz.cvut.fit.prl.scala.implicits.model._

class CallSiteExtractorSuite extends ExtractionContextSuite with ModelSimplification with ModelDSL {

  implicit val callSiteOrdering: Ordering[CallSite] = (x: CallSite, y: CallSite) =>
    implicitly[Ordering[Int]].compare(x.callSiteId, y.callSiteId)

  callSites(
    "call site count",
    """
      | package p
      | object o {
      |  trait T
      |  class C
      |
      |  val t: T = new T {} // 1
      |  val c: C = new C  // 2
      |
      |  def f(x: Seq[_]) = x.hashCode // 3
      |
      |  f(Seq(new C)) // 4, 5, 6
      | }
    """.stripMargin) { res =>
    res.callSitesCount shouldBe 6
  }

  callSites(
    "synthetic and non-synthetic implicit call site with params",
    """
      | package p
      | object o {
      |   def f()(implicit x: Int) = 1
      |   implicit val i = 1
      |
      |   f()
      |   f()(2)
      | }
    """.stripMargin) { res =>
    val expected =
      callSite("p/o.f().", "f", implicitArgumentVal("p/o.i."))

    // only the synthetic call site should be visible
    checkElementsSorted(res.callSites, List(expected))
  }

  callSites(
    "synthetic and non-synthetic implicit call site",
    """
      | package p
      | object o {
      |   1 -> "A"
      |   ArrowAssoc(2).->("B")
      | }
    """.stripMargin) { res =>
    val expected =
      callSite(
        "scala/Predef.ArrowAssoc().",
        code = "ArrowAssoc[scala/Int#](1)",
        typeArgument("scala/Int#")
      )

    checkElementsSorted(res.callSites, List(expected))
  }

  callSites(
    "simple implicit parameters",
    """
      | package p
      | object o {
      |   class A
      |   class B
      |
      |   implicit val a = new A
      |   implicit def b = new B
      |
      |   def f(x: Int)(implicit a: A, b: B) = 1
      |
      |   f(1)
      | }
    """.stripMargin) { res =>
    val expected = List(
      callSite(callSiteId = 1, declarationId = "p/o.b().", code = "b", parentCallSite(2)),
      callSite(
        callSiteId = 2,
        declarationId = "p/o.f().",
        code = "f",
        implicitArgumentVal("p/o.a."),
        implicitArgumentCall(1))
    )

    checkElementsSorted(res.callSites, expected)
  }

  callSites(
    "implicit parameter on select",
    """
      | object o {
      |   List(1).map(_.toString)
      | }
    """.stripMargin) { res =>
    val expected = List(
      callSite(
        callSiteId = 1,
        declarationId = "scala/collection/immutable/List.canBuildFrom().",
        code = "canBuildFrom[java/lang/String#]",
        typeArgument("java/lang/String#"),
        parentCallSite(2)
      ),
      callSite(
        callSiteId = 2,
        declarationId = "scala/collection/immutable/List#map().",
        code = "map[java/lang/String#, scala/collection/immutable/List#[java/lang/String#]]",
        typeArgument("java/lang/String#"),
        typeArgument("scala/collection/immutable/List#", typeRef("java/lang/String#")),
        implicitArgumentCall(1)
      )
    )

    checkElementsSorted(res.callSites, expected)
  }

  callSites(
    "orderingToOrdered",
    """
      |package p
      |
      |import scala.math.Ordered.orderingToOrdered
      |
      |case class Loc(row: Int, col: Int) {
      |  def compareTo(that: Loc): Int = {
      |    (row, col) compareTo(that.row, that.col)
      |  }
      |}
      |
    """.stripMargin
  ) { implicit res =>
    val expected = List(
      callSite(
        callSiteId = 1,
        declarationId = "scala/math/Ordering.Tuple2().",
        code = "Tuple2",
        implicitArgumentVal("scala/math/Ordering.Int."),
        implicitArgumentVal("scala/math/Ordering.Int."),
        parentCallSite(2)
      ),
      callSite(
        callSiteId = 2,
        declarationId = "scala/math/Ordered.orderingToOrdered().",
        code = "orderingToOrdered[scala/Tuple2#[scala/Int#,scala/Int#]]((row, col))",
        typeArgument("scala/Tuple2#", typeRef("scala/Int#"), typeRef("scala/Int#")),
        implicitArgumentCall(1)
      )
    )

    checkElementsSorted(res.callSites, expected)
  }

  callSites(
    ".apply with implicit parameter",
    """
      | package p
      | object o {
      |   class A
      |   class B[T](x: T)
      |
      |   object B {
      |     implicit def apply[T](x: T)(implicit y: T) = new B(y)
      |   }
      |
      |   implicit val a = new A
      |
      |   B(new A)
      | }
    """.stripMargin
  ) { res =>
    val expected = callSite(
      declarationId = "p/o.B.apply().",
      code = ".apply[p/o.A#]",
      typeArgument("p/o.A#"),
      implicitArgumentVal("p/o.a.")
    )

    checkElements(res.callSites, List(expected))
  }

  callSites(
    "implicit .apply with no implicit parameters",
    """
      | package p
      | object o {
      |   class B
      |   object B {
      |     implicit def apply(x: String) = ???
      |   }
      |
      |   B("A")
      | }
    """.stripMargin
  ) { res =>
    res.callSites shouldBe empty
  }

  callSites(
    "explicit call of implicit def with implicit parameter",
    """
      | package p
      | object o {
      |   class A
      |
      |   implicit def pipe[T](x: T)(implicit y: A) = ???
      |   implicit val a = new A
      |
      |   pipe(1)
      | }
    """.stripMargin
  ) { res =>
    val expected =
      callSite(
        declarationId = "p/o.pipe().",
        code = "pipe[scala/Int#]",
        typeArgument("scala/Int#"),
        implicitArgumentVal("p/o.a.")
      )

    checkElementsSorted(res.callSites, List(expected))
  }

  callSites(
    "explicit call of implicit def with implicit parameter and types",
    """
      | package p
      | object o {
      |   class A
      |
      |   implicit def pipe[T](x: T)(implicit y: T) = ???
      |   implicit val a = new A
      |
      |   pipe(new A)
      | }
    """.stripMargin
  ) { res =>
    val expected =
      callSite(
        declarationId = "p/o.pipe().",
        code = "pipe[p/o.A#]",
        typeArgument("p/o.A#"),
        implicitArgumentVal("p/o.a.")
      )

    checkElementsSorted(res.callSites, List(expected))
  }

  callSites(
    "explicit call of implicit def with explicit parameter",
    """
      | package p
      | object o {
      |   class A
      |
      |   implicit def pipe[T](x: T)(implicit y: A) = ???
      |   implicit val a = new A
      |
      |   pipe(1)(a)
      | }
    """.stripMargin
  ) { res =>
    res.callSites shouldBe empty
    res.callSitesCount shouldBe 2
  }

  callSites(
    "call with implicit parameter from def",
    """
      | package p
      | object o {
      |   class A
      |
      |   implicit def a: A = new A
      |   def f(x: Int)(implicit y: A) = ???
      |   f(1)
      | }
    """.stripMargin
  ) { res =>
    val expected = List(
      callSite(1, "p/o.a().", "a", parentCallSite(2)),
      callSite(2, "p/o.f().", "f", implicitArgumentCall(1))
    )

    checkElementsSorted(res.callSites, expected)
  }

  callSites(
    "call with implicit parameter from def with type argument",
    """
      | package p
      | object o {
      |  trait TC[X]
      |  class A
      |
      |  implicit val ta: TC[A] = new TC[A] {}
      |
      |  implicit def tcSeq[T](implicit ev: TC[T]): TC[Seq[T]] = new TC[Seq[T]] {}
      |
      |  def f[T](x: T)(implicit tc: TC[T]) = tc.hashCode
      |
      |  f(Seq(Seq(new A)))
      | }
    """.stripMargin
  ) { res =>
    val expected = List(
      callSite(
        callSiteId = 1,
        declarationId = "p/o.tcSeq().",
        code = "tcSeq",
        implicitArgumentVal("p/o.ta."),
        parentCallSite(2)
      ),
      callSite(
        callSiteId = 2,
        declarationId = "p/o.tcSeq().",
        code = "tcSeq",
        implicitArgumentCall(1),
        parentCallSite(3)
      ),
      callSite(
        callSiteId = 3,
        declarationId = "p/o.f().",
        code = "f[scala/collection/Seq#[scala/collection/Seq#[p/o.A#]]]",
        implicitArgumentCall(2),
        typeArgument("scala/collection/Seq#", typeRef("scala/collection/Seq#", typeRef("p/o.A#")))
      )
    )

    checkElementsSorted(res.callSites.sorted, expected)
  }

  // inspired by ensimefile.scala:44
  callSites(
    "implicit conversion and select with implicit parameter",
    """
      | package p
      | object o {
      |   class Charset
      |   implicit class RichPath(that: String) {
      |     def readString()(implicit cs: Charset) = 1
      |   }
      |
      |   object raw {
      |     val file = "file"
      |   }
      |
      |   implicit val utf = new Charset
      |
      |   raw.file.readString()
      | }
    """.stripMargin
  ) { res =>
    val expected = List(
      callSite(
        callSiteId = 2,
        declarationId = "p/o.RichPath().",
        code = "RichPath(raw.file)"
      ),
      callSite(
        callSiteId = 1,
        declarationId = "p/o.RichPath#readString().",
        code = "readString",
        implicitArgumentVal("p/o.utf.")
      )
    )

    checkElementsSorted(res.callSites, expected)
  }

  callSites(
    "implicit multi hop parameters",
    """
      | package p
      | object o {
      |   class A
      |   class B
      |   class C
      |
      |   implicit def a2b(implicit x: A) = new B
      |   implicit def b2c(implicit x: B) = new C
      |
      |   implicit def a = new A
      |
      |   def f(x: Int)(implicit c: C) = "C"
      |
      |   f(1)
      | }
    """.stripMargin
  ) { res =>
    val expected = List(
      callSite(
        callSiteId = 1,
        declarationId = "p/o.a().",
        code = "a",
        parentCallSite(2)
      ),
      callSite(
        callSiteId = 2,
        declarationId = "p/o.a2b().",
        code = "a2b",
        implicitArgumentCall(1),
        parentCallSite(3)
      ),
      callSite(
        callSiteId = 3,
        declarationId = "p/o.b2c().",
        code = "b2c",
        implicitArgumentCall(2),
        parentCallSite(4)
      ),
      callSite(
        callSiteId = 4,
        declarationId = "p/o.f().",
        code = "f",
        implicitArgumentCall(3)
      )
    )

    checkElementsSorted(res.callSites, expected)
  }

  callSites(
    "implicit conversion with parameters",
    """
      | package p
      | object o {
      |   class A
      |   class B {
      |     val x = 1
      |   }
      |
      |   implicit def a = new A
      |   implicit def c(x: Int)(implicit a: A) = new B
      |
      |   1.x
      | }
    """.stripMargin) { res =>
    val expected = List(
      callSite(
        callSiteId = 1,
        declarationId = "p/o.a().",
        code = "a",
        parentCallSite(2)
      ),
      callSite(
        callSiteId = 2,
        declarationId = "p/o.c().",
        code = "c(1)",
        implicitArgumentCall(1)
      )
    )

    checkElementsSorted(res.callSites, expected)
  }

  callSites(
    "implicit conversion with parameters and select with implicits",
    """
      | package p
      | object o {
      |   class A1
      |   class A2
      |   class B {
            def x(implicit a2: A2) = 1
      |   }
      |
      |   implicit def a1 = new A1
      |   implicit val a2 = new A2
      |   implicit def c(x: Int)(implicit a: A1) = new B
      |
      |   1.x
      | }
    """.stripMargin) { res =>
    val expected = List(
      callSite(
        callSiteId = 1,
        declarationId = "p/o.B#x().",
        code = "x",
        implicitArgumentVal("p/o.a2.")
      ),
      callSite(
        callSiteId = 2,
        declarationId = "p/o.a1().",
        code = "a1",
        parentCallSite(3)
      ),
      callSite(
        callSiteId = 3,
        declarationId = "p/o.c().",
        code = "c(1)",
        implicitArgumentCall(2)
      )
    )

    checkElementsSorted(res.callSites, expected)
  }

  callSites(
    "implicit conversion with parameters and type parameters",
    """
      | package p
      | object o {
      |   class A[T]
      |
      |   class B {
      |     val x = 1
      |   }
      |
      |   implicit def a[T] = new A[T]
      |   implicit def c[T](x: T)(implicit a: A[T]) = new B
      |
      |   1.x
      | }
    """.stripMargin
  ) { res =>
    val expected = List(
      callSite(
        callSiteId = 1,
        declarationId = "p/o.a().",
        code = "a[scala/Int#]",
        typeArgument("scala/Int#"),
        parentCallSite(2)
      ),
      callSite(
        callSiteId = 2,
        declarationId = "p/o.c().",
        code = "c[scala/Int#](1)",
        typeArgument("scala/Int#"),
        implicitArgumentCall(1)
      )
    )

    checkElementsSorted(res.callSites, expected)
  }

  callSites(
    "for-comprehension",
    """
      | object o {
      |   for {
      |     i <- Seq(1,2)
      |   } yield "A" + i
      | }
    """.stripMargin) { res =>
    val expected = List(
      callSite(
        callSiteId = 1,
        declarationId = "scala/collection/Seq.canBuildFrom().",
        code = "canBuildFrom[java/lang/String#]",
        typeArgument("java/lang/String#"),
        parentCallSite(2)
      ),
      callSite(
        callSiteId = 2,
        declarationId = "scala/collection/TraversableLike#map().",
        code = ".map[java/lang/String#, scala/collection/Seq#[java/lang/String#]]",
        typeArgument("java/lang/String#"),
        typeArgument("scala/collection/Seq#", typeRef("java/lang/String#")),
        implicitArgumentCall(1)
      )
    )

    checkElementsSorted(res.callSites, expected)
  }

  callSites(
    "for-comprehension with filter",
    """
      | object o {
      |   for {
      |     i <- 1 to 10 if i % 2 == 0
      |     j <- 2 until i
      |   } yield (i, j)
      | }
    """.stripMargin
  ) { res =>
    val expected = List(
      callSite(
        callSiteId = 1,
        declarationId = "scala/collection/immutable/IndexedSeq.canBuildFrom().",
        code = "canBuildFrom[scala/Tuple2#[scala/Int#,scala/Int#]]",
        typeArgument("scala/Tuple2#", typeRef("scala/Int#"), typeRef("scala/Int#")),
        parentCallSite(4)
      ),
      callSite(
        callSiteId = 2,
        declarationId = "scala/collection/immutable/IndexedSeq.canBuildFrom().",
        code = "canBuildFrom[scala/Tuple2#[scala/Int#,scala/Int#]]",
        typeArgument("scala/Tuple2#", typeRef("scala/Int#"), typeRef("scala/Int#")),
        parentCallSite(3)
      ),
      callSite(
        callSiteId = 3,
        declarationId = "scala/collection/TraversableLike#map().",
        code =
          ".map[scala/Tuple2#[scala/Int#,scala/Int#], scala/collection/immutable/IndexedSeq#[scala/Tuple2#[scala/Int#,scala/Int#]]]",
        typeArgument("scala/Tuple2#", typeRef("scala/Int#"), typeRef("scala/Int#")),
        typeArgument(
          "scala/collection/immutable/IndexedSeq#",
          typeRef("scala/Tuple2#", typeRef("scala/Int#"), typeRef("scala/Int#"))),
        implicitArgumentCall(2),
        parentCallSite(4)
      ),
      callSite(
        callSiteId = 4,
        declarationId = "scala/collection/generic/FilterMonadic#flatMap().",
        code =
          ".flatMap[scala/Tuple2#[scala/Int#,scala/Int#], scala/collection/immutable/IndexedSeq#[scala/Tuple2#[scala/Int#,scala/Int#]]]",
        typeArgument("scala/Tuple2#", typeRef("scala/Int#"), typeRef("scala/Int#")),
        typeArgument(
          "scala/collection/immutable/IndexedSeq#",
          typeRef("scala/Tuple2#", typeRef("scala/Int#"), typeRef("scala/Int#"))),
        implicitArgumentCall(1)
      ),
      callSite(
        callSiteId = 6,
        declarationId = "scala/LowPriorityImplicits#intWrapper().",
        code = "intWrapper(1)"
      ),
      callSite(
        callSiteId = 7,
        declarationId = "scala/LowPriorityImplicits#intWrapper().",
        code = "intWrapper(2)"
      )
    )

    checkElementsSorted(res.callSites, expected)
  }

  callSites(
    "for-comprehension with features",
    """
      | object o {
      |   import scala.concurrent.Future
      |   import scala.concurrent.ExecutionContext.Implicits.global
      |
      |   for {
      |     a <- Future(1)
      |     b <- Future(2)
      |   } yield a + b
      |
      | }
    """.stripMargin
  ) { res =>
    val expected = List(
      callSite(
        callSiteId = 1,
        declarationId = "scala/concurrent/Future#map().",
        code = ".map[scala/Int#]",
        typeArgument("scala/Int#"),
        implicitArgumentVal("scala/concurrent/ExecutionContext.Implicits.global."),
        parentCallSite(3)
      ),
      callSite(
        callSiteId = 2,
        declarationId = "scala/concurrent/Future.apply().",
        code = ".apply[scala/Int#]",
        typeArgument("scala/Int#"),
        implicitArgumentVal("scala/concurrent/ExecutionContext.Implicits.global.")
      ),
      callSite(
        callSiteId = 3,
        declarationId = "scala/concurrent/Future#flatMap().",
        code = ".flatMap[scala/Int#]",
        typeArgument("scala/Int#"),
        implicitArgumentVal("scala/concurrent/ExecutionContext.Implicits.global.")
      ),
      callSite(
        callSiteId = 4,
        declarationId = "scala/concurrent/Future.apply().",
        code = ".apply[scala/Int#]",
        typeArgument("scala/Int#"),
        implicitArgumentVal("scala/concurrent/ExecutionContext.Implicits.global.")
      )
    )

    checkElementsSorted(res.callSites, expected)
  }

  callSites(
    "implicit argument in a constructor",
    """
      | package p
      | object o {
      |   import scala.concurrent.ExecutionContext
      |   import scala.concurrent.ExecutionContext.Implicits.global
      |
      |   class A[T](x: T)(implicit e: ExecutionContext)
      |
      |   new A(1)
      | }
    """.stripMargin
  ) { res =>
    val expected = callSite(
      declarationId = "p/o.A#`<init>`().",
      code = "<init>",
      implicitArgumentVal("scala/concurrent/ExecutionContext.Implicits.global.")
    )

    checkElementsSorted(res.callSites, List(expected))
  }

  callSites(
    "implicit argument in a case class",
    """
      | package p
      | object o {
      |   import scala.concurrent.ExecutionContext
      |   import scala.concurrent.ExecutionContext.Implicits.global
      |
      |   case class B[T](x: T)(implicit e: ExecutionContext)
      |
      |   B(1)
      | }
    """.stripMargin
  ) { res =>
    val expected = callSite(
      declarationId = "p/o.B.apply().",
      code = ".apply[scala/Int#]",
      typeArgument("scala/Int#"),
      implicitArgumentVal("scala/concurrent/ExecutionContext.Implicits.global.")
    )

    checkElementsSorted(res.callSites, List(expected))
  }

  callSites(
    ".apply with no implicit parameters",
    """
      | package p
      | object o {
      |   class B
      |   object B {
      |     def apply(x: String) = ???
      |   }
      |
      |   B("A")
      | }
    """.stripMargin
  ) { res =>
    res.callSites shouldBe empty
  }

  callSites(
    ".apply with explicit implicit parameter",
    """
      | package p
      | object o {
      |   class B
      |   object B {
      |     def apply(x: String)(implicit y: Int) = ???
      |   }
      |
      |   B("A")(1)
      | }
    """.stripMargin
  ) { res =>
    res.callSites shouldBe empty
  }

  callSites("string interpolation with map",
    """
      | package p
      | object o {
      |   val xs = Seq(1)
      |   s"Test ${xs.map(_ + 1)}"
      | }
    """.stripMargin) { res =>
    val expected = List(
      callSite(
        callSiteId = 1,
        declarationId = "scala/collection/Seq.canBuildFrom().",
        code = "canBuildFrom[scala/Int#]",
        typeArgument("scala/Int#"),
        parentCallSite(2)
      ),
      callSite(
        callSiteId = 2,
        declarationId = "scala/collection/TraversableLike#map().",
        code="map[scala/Int#, scala/Any#]",
        typeArgument("scala/Int#"),
        typeArgument("scala/Any#"), // TODO: why is it Any?
        implicitArgumentCall(1)
      )
    )

    checkElementsSorted(res.callSites, expected)
  }

  callSites("auxiliary constructors with implicits",
    """
      | package p
      | object o {
      |   class A1
      |   class A2
      |
      |   class B(x: Int) {
      |     def this(x: Int, y: Int)(implicit z: A1) = this(x)
      |     def this(x: String)(implicit z: A2) = this(1)
      |   }
      |
      |   implicit val a1 = new A1
      |   implicit val a2 = new A2
      |
      |   new B(1)
      |   new B(1,2)
      |   new B("A")
      | }
    """.stripMargin) { res =>
    val expected = List(
      callSite(
        callSiteId = 1,
        declarationId = "p/o.B#`<init>`(+1).",
        code = "<init>",
        implicitArgumentVal("p/o.a1.")
      ),
      callSite(
        callSiteId = 2,
        declarationId = "p/o.B#`<init>`(+2).",
        code = "<init>",
        implicitArgumentVal("p/o.a2.")
      )
    )

    checkElementsSorted(res.callSites, expected)
  }

  callSites("overwritten methods",
    """
      | package p
      | object o {
      |  class A
      |
      |  def f(x: Int)(implicit y: A) = 1
      |  def f(x: String)(implicit y: A) = 2
      |
      |  implicit val a = new A
      |
      |  f(1)
      |  f("A")
      | }
    """.stripMargin) { res =>
    val expected = List(
      callSite(
        callSiteId = 1,
        declarationId = "p/o.f().",
        code = "f",
        implicitArgumentVal("p/o.a.")
      ),
      callSite(
        callSiteId = 2,
        declarationId = "p/o.f(+1).",
        code = "f",
        implicitArgumentVal("p/o.a.")
      )
    )

    checkElementsSorted(res.callSites, expected)
  }
}

//  def checkNoFailures(extractor: CallSiteExtractor): Unit = {
//    extractor.failures.foreach(_.printStackTrace())
//    extractor.failures shouldBe empty
//  }
//
//  // TC
//  //  database(
//  //    """
//  //      |object X {
//  //      |  trait Jsonable[-T] {
//  //      |    def toJson(x: T): String
//  //      |  }
//  //      |
//  //      |  implicit val int2jsonable: Jsonable[Int] =  { x: Int => x.toString }
//  //      |
//  //      |  implicit def traversable2jsonable[T: Jsonable] = new Jsonable[Traversable[T]] {
//  //      |    override def toJson(x: Traversable[T]): String = {
//  //      |      val tc = implicitly[Jsonable[T]]
//  //      |      x.map(tc.toJson).mkString("[", ",", "]")
//  //      |    }
//  //      |  }
//  //      |
//  //      |  implicit class XtensionJson[T: Jsonable](x: T) {
//  //      |    def toJson: String = implicitly[Jsonable[T]].toJson(x)
//  //      |  }
//  //      |
//  //      |  //def json[T](x: T)(implicit e: Jsonable[T]): String = {
//  //      |  //  e.toJson(x)
//  //      |  //}
//  //      |
//  //      |  Seq(1,2,3).toJson
//  //      |  //json(Seq(1,2,3))
//  //      |}
//  //    """.stripMargin
//  //  ) { db =>
//  //  }
//  //
//
//  //  database(
//  //    """
//  //      |object X {
//  //      |  Seq(1) ++ Seq(2)
//  //      |}
//  //    """.stripMargin) { db =>
//  //    val cs = CallSite.extractCallSites(db.text.parse[Source].get)
//  //    println(cs.mkString("\n\n"))
//  //    println(db.synthetics.map(x => x -> new LegacySyntheticPrinter().toLegacy(x)).mkString("\n\n"))
//  //    println()
//  //  }
//
//  //    println(cs.mkString("\n\n"))
//  //    println(synthetics.map(x => x -> new LegacySyntheticPrinter().toLegacy(x)).mkString("\n\n"))
//  //    println()
//
//  def extraction(code: String)(fn: CallSiteExtractor => Unit): Unit = {
//    implicit object RangeSorted extends Ordering[s.Range] {
//      override def compare(x: s.Range, y: s.Range): Int = {
//        val d = x.startLine.compare(y.startLine)
//        if (d == 0) x.startCharacter.compare(y.startCharacter) else d
//      }
//    }
//
//    database(code)(db => {
//      val extractor = new CallSiteExtractor(db, symtab)
//      checkNoFailures(extractor)
//      fn(extractor)
//    })
//  }
//
//  extraction(
//    """
//      |object InferredApplyCall {
//      |  Seq(1)
//      |}
//    """.stripMargin) { extractor =>
//
//  }
//
////  extraction(
////    """
////      | object NestedCall {
////      |   math.max(1+2, 3+4)
////      | }
////    """.stripMargin) { extractor =>
////    val css = extractor.callSites
////    css should have size 3
////  }
////
////  //  extraction(
////  //    """
////  //      |
////  //    """.stripMargin) { extractor =>
////  //
////  ////    class A
////  ////    class B
////  ////    implicit def a2b(x: A): B = new B
////  ////    implicit val a = new A
////  ////    def f(x: Int)(implicit b: B) = 1
////  ////
////  ////    f(1)
////  //  }
////
////  extraction(
////    """
////      |
////    """.stripMargin
////  ) { extraction =>
////    extraction.callSites shouldBe empty
////  }
////
////  extraction(
////    """
////      |object NestedCallsInParams {
////      |  def a(x: Int) = 1
////      |  def b(x: Int) = 2
////      |  def c(x: Int) = 3
////      |  def f(x: Int, y: Int) = 5
////      |  object d {
////      |    def e(x: Int) = 6
////      |  }
////      |
////      |  f(a(b(c(4))), d.e(c(7)))
////      |}
////      |
////    """.stripMargin
////  ) { extraction =>
////    extraction.callSites should have size 6
////  }
////
////  extraction(
////    """
////      | object NonLocalImplicitParameter {
////      |   Seq(1) ++ List(1)
////      | }
////    """.stripMargin) { extractor =>
////
////    val css = extractor.callSites
////    css should have size 4
////
////    css.find(_.declaration.name == "canBuildFrom") check[SyntheticCall] { x =>
////      x.typeArgs should have size 1
////      x.typeArgs.head.check[TypeRef] { x =>
////        x.symbol.displayName shouldBe "Int"
////      }
////    }
////    css.find(_.code == "List(1)") check[NormalCall] { x =>
////      x.typeArgs should have size 1
////    }
////    css.find(_.code == "Seq(1)") check[NormalCall] { x =>
////      x.typeArgs should have size 1
////    }
////    css.find(_.declaration.name == "++") check[NormalCall] { x =>
////      x.implicitArgs.check[ArgumentsList] { x =>
////        x.args should have size 1
////        x.args.head.check[CallSiteRef] { x =>
////          x.callSite shouldBe css.find(_.declaration.name == "canBuildFrom").get
////        }
////      }
////      x.typeArgs should have size 2
////    }
////  }
////
////  // TODO:  "object X { val x: a.b = c.d }"
////  // TODO:  type +[A,B] = Tupel2[A,B]; val x: A+B = 1
////
//////  extraction(
//////    """
//////      |object X {
//////      |  def f[T](x: T) = x
//////      |  f(1)
//////      |}
//////    """.stripMargin) { extractor =>
//////
//////    val css = extractor.callSites
//////    css should have size 1
//////
//////    css.find(_.declaration.name == "f") check[NormalCall] { x =>
//////      x.typeArgs should have size 1
//////    }
//////
//////    import scala.math.Ordered.orderingToOrdered
//////
//////    val x: java.io.File = ???
//////    val y = 42
//////    (x, y)
//////      .compare((x, y))
//////
//////  }
////
////
////  extraction(
////    """
////      |object X {
////      |  object A {
////      |    def apply(x: Int) = x
////      |  }
////      |  A(1)
////      |}
////    """.stripMargin) { extractor =>
////
////    val css = extractor.callSites
////    css should have size 1
////
////    css.find(_.declaration.name == "apply") check[NormalCall] { x =>
////      x.argss should have size 1
////      x.argss.head.args should have size 1
////    }
////  }
////
////  extraction(
////    """
////      |object X {
////      |  object A {
////      |    def apply(x: String)(implicit y: Int) = x
////      |  }
////      |  implicit val iy = 1
////      |  A("A")
////      |}
////    """.stripMargin) { extractor =>
////
////    val css = extractor.callSites
////    css should have size 1
////
////    css.find(_.declaration.name == "apply") check[NormalCall] { x =>
////      x.implicitArgs.check[ArgumentsList] { list =>
////        list.syntactic shouldBe false
////        list.args should have size 1
////      }
////    }
////  }
////
////  extraction(
////    """
////      |object X {
////      |  object A {
////      |    def apply[T](x: T)(implicit y: T) = x
////      |  }
////      |  implicit val iy = 1
////      |  A(1)
////      |}
////    """.stripMargin) { extractor =>
////    extractor.failures shouldBe empty
////
////    val css = extractor.callSites
////    css should have size 1
////
////    css.find(_.declaration.name == "apply") check[NormalCall] { x =>
////      x.implicitArgs.check[ArgumentsList] { list =>
////        list.syntactic shouldBe false
////        list.args should have size 1
////      }
////      x.typeArgs should have size 1
////    }
////  }
////
////  extraction(
////    """
////      |object X1 {
////      |  object A {
////      |    def apply[T](x: T)(implicit y: T) = x
////      |  }
////      |  implicit val iy = 1
////      |  A.apply[Int](1)
////      |}
////    """.stripMargin) { extractor =>
////
////    val css = extractor.callSites
////    css should have size 1
////
////    css.find(_.declaration.name == "apply") check[NormalCall] { x =>
////      x.implicitArgs.check[ArgumentsList] { list =>
////        list.syntactic shouldBe false
////        list.args should have size 1
////      }
////      x.typeArgs should have size 1
////    }
////  }
////
////  extraction(
////    """
////      |object X2 {
////      |  object A {
////      |    def apply[T](x: T)(implicit y: T) = x
////      |  }
////      |  implicit val iy = 1
////      |  A.apply[Int](1)(2)
////      |}
////    """.stripMargin) { extractor =>
////
////    val css = extractor.callSites
////    css should have size 1
////
////    css.find(_.declaration.name == "apply") check[NormalCall] { x =>
////      x.implicitArgs.check[ArgumentsList] { list =>
////        list.syntactic shouldBe true
////        list.args should have size 1
////      }
////      x.typeArgs should have size 1
////    }
////  }
////
////  extraction(
////    """
////      |object X2 {
////      |  class A(x: Int)(y: String)(implicit z: Boolean)
////      |  implicit val iz = true
////      |  new A(1)("A")
////      |}
////    """.stripMargin) { extractor =>
////
////    val css = extractor.callSites
////    css should have size 1
////
////    css.find(_.declaration.name == "<init>") check[NormalCall] { x =>
////      x.implicitArgs.check[ArgumentsList] { list =>
////        list.syntactic shouldBe false
////        list.args should have size 1
////      }
////      x.typeArgs should have size 0
////    }
////  }
////
////  extraction(
////    """
////      |object ClassConstructorImplicitTypeArgs {
////      |  class A[T](x: T)(y: String)(implicit z: Boolean)
////      |  implicit val iz = true
////      |  new A(1)("A")
////      |}
////    """.stripMargin) { extractor =>
////
////    val css = extractor.callSites
////    css should have size 1
////
////    css.find(_.declaration.name == "<init>") check[NormalCall] { x =>
////      x.implicitArgs.check[ArgumentsList] { list =>
////        list.syntactic shouldBe false
////        list.args should have size 1
////      }
////      // there are no type arguments, only parameters
////      x.typeArgs should have size 0
////    }
////  }
////
////  //  class X
////  //  class X2 extends X
////
////  extraction(
////    """
////      |object ClassConstructorExplicitImplicitTypeArgs {
////      |  class A[T](x: T)(y: String)(implicit z: Boolean)
////      |  implicit val iz = true
////      |  new A[Any](1)("A")(false)
////      |}
////    """.stripMargin) { extractor =>
////    extractor.failures shouldBe empty
////
////    val css = extractor.callSites
////    css should have size 1
////
////    css.find(_.declaration.name == "<init>") check[NormalCall] { x =>
////      x.implicitArgs.check[ArgumentsList] { list =>
////        list.syntactic shouldBe true
////        list.args should have size 1
////      }
////      x.typeArgs should have size 1
////    }
////  }
////
////  extraction(
////    """
////      | object ExpressionsArguments {
////      |   def f(x: Int) = x
////      |   f(1+1)
////      | }
////    """.stripMargin
////  ) { extractor =>
////    val css = extractor.callSites
////    css should have size 2
////
////  }
////
////  extraction(
////    """
////      | object BasicStringInterpolation {
////      |   val z = "A"
////      |   s"Test $z"
////      | }
////    """.stripMargin
////  ) { extractor =>
////    val css = extractor.callSites
////    css should have size 1
////
////  }
////
////  // FIXME: making z a string does not work - "java/lang/String#`+`()." cannot be resolved in symtab?!
////  extraction(
////    """
////      | object StringInterpolationWithExpression {
////      |   val z = 1 // "A"
////      |   s"Test ${z + 1}"
////      | }
////    """.stripMargin
////  ) { extractor =>
////    val css = extractor.callSites
////    css should have size 2
////  }
////
////  extraction(
////    """
////      | object StringInterpolationWithMap {
////      |   val xs = Seq(1)
////      |   s"Test ${xs.map(_ + 1)}"
////      | }
////    """.stripMargin
////  ) { extractor =>
////    val css = extractor.callSites
////    css should have size 5
////  }
////
////  extraction(
////    """
////      |package a.b.c
////      |
////      |object SelectInPackage {
////      |}
////    """.stripMargin
////  ) { extractor => extractor.callSites shouldBe empty }
////
////  extraction(
////    """
////      |import scala.concurrent.Future
////      |
////      |object SelectInImport {
////      |}
////    """.stripMargin
////  ) { extractor => extractor.callSites shouldBe empty }
////
////  extraction(
////    """
////      |object SelectInVar {
////      |  var x: scala.collection.Seq[Int] = null
////      |}
////    """.stripMargin) { extractor =>
////    extractor.callSites shouldBe empty
////  }
////
////  extraction(
////    """
////      |object SelectInVal {
////      |  val x: scala.collection.Seq[Int] = null
////      |}
////    """.stripMargin) { extractor =>
////    extractor.callSites shouldBe empty
////  }
////
////  extraction(
////    """
////      |object SelectInParam {
////      |  def f(x: scala.collection.Seq[Int]): scala.collection.Seq[Int] = x
////      |}
////    """.stripMargin) { extractor =>
////    extractor.callSites shouldBe empty
////  }
////
////  extraction(
////    """
////      |object SelectAsCall {
////      |  "A".hashCode.hashCode
////      |}
////    """.stripMargin) { extractor =>
////    val css = extractor.callSites
////    css should have size 2
////  }
////
//////    extraction(
//////      """
//////        |object CompareOnTuple {
//////        |  import scala.math.Ordered.orderingToOrdered
//////        |
//////        |  val f: java.io.File = ???
//////        |  val i: Int = 1
//////        |  val x =
//////        |    (f, i)
//////        |      .compare((f, i))
//////        |}
//////      """.stripMargin
//////    ) { extractor =>
//////      val css = extractor.callSites
//////      css should have size 4
//////    }
////
////  // TODO: new A[]
////  // TODO: new A[](impl)
////  // TODO: new A[] {}
////  // TODO: new A[](impl) {}
////  // TODO: a.+[A](impl)
////  // TODO: -[A]x(impl)
////  // TODO: multiple parameter lists
////
////  //
////  //  extraction(
////  //    """
////  //      |object X {
////  //      |  val List(a,b) = Seq(1,2)
////  //      |}
////  //    """.stripMargin) { extractor =>
////  //
////  //    val css = extractor.callSites
////  //    css should have size 3
////  //
////  //    // TOD0: there should be two apply
////  //    css.find(_.fun.name == "apply") check { x =>
////  //      x shouldBe a[NormalCall]
////  //    }
////  //    css.find(_.fun.name == "unapplySeq") check { x =>
////  //      x shouldBe a[SyntheticCall]
////  //    }
////  //  }
////  //
////  //  extraction(
////  //    """
////  //      |object X {
////  //      |  1 -> 2
////  //      |}
////  //    """.stripMargin) { extractor =>
////  //    extractor.failues shouldBe empty
////  //
////  //    val css = extractor.callSites
////  //    css should have size 2
////  //
////  //    css.find(_.fun.name == "ArrowAssoc") check { x =>
////  //      x shouldBe a[ConversionCall]
////  //    }
////  //    css.find(_.fun.name == "->") check { x =>
////  //      x shouldBe a[NormalCall]
////  //    }
////  //  }
////  //
////  //  extraction(
////  //    """
////  //      |object X {
////  //      |  "hi".stripMargin
////  //      |}
////  //    """.stripMargin) { extractor =>
////  //    extractor.failues shouldBe empty
////  //
////  //    val css = extractor.callSites
////  //    css should have size 2
////  //
////  //    css.find(_.fun.name == "stripMargin") check { x =>
////  //      x shouldBe a[NormalCall]
////  //    }
////  //    css.find(_.fun.name == "augmentString") check { x =>
////  //      x shouldBe a[ConversionCall]
////  //    }
////  //  }
////
////  //  extraction(
////  //    """
////  //      |object X {
////  //      |  List(1).map(_ + 2)
////  //      |}
////  //    """.stripMargin) { extractor =>
////  //    extractor.failues shouldBe empty
////  //
////  //    val css = extractor.callSites
////  //    println(css)
////  //
////  //    css should have size 3
////  //
////  //  }
////
////  extraction(
////    """
////      |object SimpleFor {
////      |  for {
////      |    i <- Seq(1,2)
////      |  } yield i.toString
////      |}
////    """.stripMargin) { extractor =>
////
////    val css = extractor.callSites
////
////    css should have size 4
////
////    css.find(_.declaration.name == "apply") check[NormalCall] { x =>
////    }
////    css.find(_.declaration.name == "toString") check[NormalCall] { x =>
////    }
////    css.find(_.declaration.name == "map") check[SyntheticCall] { x =>
////      x.argss should have size 2
////      x.implicitArgs.check[ArgumentsList] { x =>
////        x.syntactic shouldBe false
////        x.args should have size 1
////        x.args.head.check[CallSiteRef] { x =>
////          x.callSite.declaration.name shouldBe "canBuildFrom"
////        }
////      }
////    }
////  }
////
////  //    Seq(1,2) map (i=> i + 1)
////
////  extraction(
////    """
////      |object NestForWithFilter {
////      |  for {
////      |    i <- 1 to 10 if i % 2 == 0
////      |    j <- Seq('A', 'B')
////      |  } yield (i, j)
////      |}
////    """.stripMargin) { extractor =>
////
////    val css = extractor.callSites
////
////    css should have size 10
////  }
////
////  extraction(
////    """
////      |object ForLoopWithFuture {
////      |  import scala.concurrent.Future
////      |  import scala.concurrent.ExecutionContext.Implicits.global
////      |
////      |  for {
////      |    a <- Future.successful(1)
////      |    b <- Future.successful(2)
////      |  } a + b
////      |}
////    """.stripMargin) { extractor =>
////    val css = extractor.callSites
////
////    css should have size 5
////  }
////
////  // TODO:
////  //     (1 to 10).withFilter(i => i % 2 == 0).flatMap(i =>
////  //      Seq('A','B').map(j => (i,j))
////  //    )
////
////  // TODO: test on explicit nested calls like f(a[A](b[B#C](c.d[C])), d(e(),f(g())))
////
////  //
////  //  extraction(
////  //    """
////  //      |object X {
////  //      |  List(1)
////  //      |}
////  //    """.stripMargin) { extractor =>
////  //    extractor.failues shouldBe empty
////  //
////  //    val css = extractor.callSites
////  //    css should have size (1)
////  //
////  //    val cs = css.head
////  //    cs shouldBe a[NormalCall]
////  //    cs.fun shouldBe "List"
////  //  }
////  //
////  //  extraction(
////  //    """
////  //      |object X {
////  //      |  val List(a,b) = Seq(1,2)
////  //      |}
////  //    """.stripMargin) { extractor =>
////  //    extractor.failues shouldBe empty
////  //
////  //    val css = extractor.callSites
////  //    css should have size (1)
////  //
////  //    val cs = css.head
////  //    cs shouldBe a[NormalCall]
////  //    cs.fun shouldBe "Seq"
////  //  }
////
////  // "fooo".stripPrefix("o")
////  // 1 #:: 2 #:: Stream.empty
////  // Array.empty[Int]
////  // for {
////  //  i <- 1 to 10
////  //  j <- 0 until 10
////  //  if i % 2 == 0
////  //} yield (i, j)
////  // for {
////  //   a <- Future.successful(1)
////  //   b <- Future.successful(2)
////  // } println(a)
////
////  //  database(
////  //    """
////  //      | object X {
////  //      |   val a = 1
////  //      |   s"$a + 1"
////  //      |   s"${a + 1}"
////  //      |   s"${a.hashCode}"
////  //      | }
////  //    """.stripMargin) { db =>
////  //
////  //    val tree = db.text.parse[Source].get
////  //    println(CallSite.extractCallSites(tree).mkString("\n\n"))
////  //  }
////
////  //  database(
////  //    """
////  //      | object X {
////  //      |   val x = new StringBuffer
////  //      |   val y = new StringBuffer()
////  //      |   val z = new Runnable() {
////  //      |   }
////  //      | }
////  //    """.stripMargin) { db =>
////  //
////  //    val tree = db.text.parse[Source].get
////  //    println(CallSite.extractCallSites(tree).mkString("\n\n"))
////  //  }
////
////  //  database(
////  //    """
////  //      |object X {
////  //      |  //Seq(1).map(_+1).mkString(",")
////  //      |  ("A"+"B").getBytes().map(_+1).mkString(",").length.hashCode
////  //      |  Seq(1).size.toString()
////  //      |}
////  //    """.stripMargin
////  //  ) { db =>
////  //
////  //    val tree = db.text.parse[Source].get
////  //    println(CallSite.extractCallSites(tree).mkString("\n\n"))
////  //    //println(db.synthetics.map(x => x -> new LegacySyntheticPrinter().toLegacy(x)).mkString("\n\n"))
////  //  }
////
////  //  database(
////  //    """
////  //      |object X {
////  //      |  Seq(1,2,3)
////  //      |}
////  //    """.stripMargin
////  //  ) { db =>
////  //    print(db.synthetics)
////  //  }
////
//////  extraction(
//////    """
//////      |object X {
//////      |  trait Jsonable[-T] {
//////      |    def toJson(x: T): String
//////      |  }
//////      |
//////      |  implicit val int2jsonable: Jsonable[Int] =  { x: Int => x.toString }
//////      |
//////      |  implicit def traversable2jsonable[T: Jsonable] = new Jsonable[Traversable[T]] {
//////      |    override def toJson(x: Traversable[T]): String = {
//////      |      val tc = implicitly[Jsonable[T]]
//////      |      x.map(tc.toJson).mkString("[", ",", "]")
//////      |    }
//////      |  }
//////      |
//////      |  implicit class XtensionJson[T: Jsonable](x: T) {
//////      |    def toJson: String = implicitly[Jsonable[T]].toJson(x)
//////      |  }
//////      |
//////      |  //def json[T](x: T)(implicit e: Jsonable[T]): String = {
//////      |  //  e.toJson(x)
//////      |  //}
//////      |
//////      |  Seq(1,2,3).toJson
//////      |  //json(Seq(1,2,3))
//////      |}
//////    """.stripMargin) { extractor =>
//////      val css = extractor.callSites
//////    }
//}
