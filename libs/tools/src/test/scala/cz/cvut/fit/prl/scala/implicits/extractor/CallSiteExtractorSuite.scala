package cz.cvut.fit.prl.scala.implicits.extractor

import cz.cvut.fit.prl.scala.implicits.model._

class CallSiteExtractorSuite extends ExtractionContextSuite with ModelSimplification with ModelDSL {

  callSites(
    "count infix calls",
    """
      | package p
      | object o {
      |   class A {
      |     def +[T](x: T) = 1
      |   }
      |
      |   1 :: 2 :: Nil // 1, 2
      |
      |   (new A) + 1 // 3, 4
      | }
    """.stripMargin) { res =>
    res.callSitesCount shouldBe 4
  }

  callSites(
    "count unary calls",
    """
      | package p
      | object o {
      |   class A {
      |     def unary_-[T <: A](implicit x: T): Int = 1
      |   }
      |
      |   implicit val a = new A // 1
      |
      |   (-new A) // 2, 3
      | }
    """.stripMargin) { res =>
    res.callSitesCount shouldBe 3
  }

  callSites(
    "count select with type",
    """
      | package p
      | object o {
      |   "A".isInstanceOf[String]
      | }
    """.stripMargin) { res =>
    res.callSitesCount shouldBe 1
  }

  callSites(
    "count multiple parameter lists",
    """
      | package p
      | object o {
      |   def f(x: Int)(y: Int)(z: Int) = 1
      |   f(1)(2)(3)
      | }
    """.stripMargin) { res =>
    res.callSitesCount shouldBe 1
  }

  callSites(
    "count multiple parameter lists and type",
    """
      | package p
      | object o {
      |   def f[T](x: T)(y: T)(z: T) = 1
      |   f(1)(2)(3)
      | }
    """.stripMargin) { res =>
    res.callSitesCount shouldBe 1
  }

  callSites(
    "count constructors",
    """
      | package p
      | object o {
      |   class A
      |   class B(x: Int)
      |   class C(x: Int)(y: Int)(z: A)
      |   trait T
      |
      |   new A
      |   new B(1)
      |   new C(1)(2)(new A)
      |   new T {
      |     new A
      |   }
      | }
    """.stripMargin) { res =>
    res.callSitesCount shouldBe 6
  }

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
    "count assign",
    """
      | package p
      | object o {
      |   class A(var p: Int) {
      |
      |   }
      |
      |   class B {
      |     private var _p: Int = 0
      |     def p = _p
      |     def p_=(x: Int) {
      |       this._p = x // 1
      |     }
      |   }
      |
      |   val a = new A(1) // 2
      |   a.p = a.p + 1 // 3 4 5
      |
      |   val b = new B // 6
      |   b.p = b.p + 1 // 7 8 9
      | }
    """.stripMargin) { res =>
    res.callSitesCount shouldBe 9
  }

  // IGNORED till #33 is resolved
//  callSites("constructor with implicits with comment",
//    """
//      | package p
//      | object o {
//      |   class C(implicit i: Int)
//      |   implicit val i = 1
//      |   val c = (
//      |    (new
//      |     /* a comment */
//      |     C)
//      |   )
//      | }
//    """.stripMargin) { res =>
//    val expected =
//      callSite("p/o.C#`<init>`().", "<init>", implicitArgumentVal("p/o.i."))
//
//    checkElementsSorted(res.callSites, List(expected))
//
//    res.callSitesCount shouldBe 1
//  }
//
//  callSites("constructor with implicits with parens",
//    """
//      | package p
//      | object o {
//      |   class C(implicit i: Int)
//      |   implicit val i = 1
//      |   val c = ((new C))
//      | }
//    """.stripMargin) { res =>
//    val expected =
//      callSite("p/o.C#`<init>`().", "<init>", implicitArgumentVal("p/o.i."))
//
//    checkElementsSorted(res.callSites, List(expected))
//
//    res.callSitesCount shouldBe 1
//  }

  callSites(
    "call with parens",
    """
      | package p
      | object o {
      |   def f[T](x: T)(implicit y: T) = 1
      |   def g[T](x: T)(implicit y: T) = 1
      |
      |   implicit val i = 1
      |   (f((g((1)))))
      | }
    """.stripMargin) { res =>
    val expected = List(
      callSite(
        callSiteId = 1,
        declarationId = "p/o.f().",
        code = "f[scala/Int#]",
        typeArgument("scala/Int#"),
        implicitArgumentVal("p/o.i.")
      ),
      callSite(
        callSiteId = 2,
        declarationId = "p/o.g().",
        code = "g[scala/Int#]",
        typeArgument("scala/Int#"),
        implicitArgumentVal("p/o.i.")
      )
    )

    checkElementsSorted(res.callSites, expected)

    res.callSitesCount shouldBe 2
  }

  callSites(
    "postfix calls",
    """
      | package p
      | object o {
      |   List(1,2,3) sum
      | }
    """.stripMargin) { res =>
    val expected =
      callSite(
        declarationId = "scala/collection/TraversableOnce#sum().",
        code = "sum",
        implicitArgumentVal("scala/math/Numeric.IntIsIntegral.")
      )

    checkElementsSorted(res.callSites, List(expected))

    res.callSitesCount shouldBe 2
  }

  callSites(
    "postfix calls with parens",
    """
      | package p
      | object o {
      |   ((List(1,2,3)) sum)
      | }
    """.stripMargin) { res =>
    val expected =
      callSite(
        declarationId = "scala/collection/TraversableOnce#sum().",
        code = "sum",
        implicitArgumentVal("scala/math/Numeric.IntIsIntegral.")
      )

    checkElementsSorted(res.callSites, List(expected))

    res.callSitesCount shouldBe 2
  }

  callSites(
    "Unary methods",
    """
      | package p
      | object o {
      |   class A {
      |     def unary_-[T <: A](implicit x: T): Int = -1
      |   }
      |
      |   def f[T](x: T)(implicit y: T) = 2
      |
      |   implicit val a = new A
      |   implicit val i = 1
      |   val a1 = new A
      |
      |   -a1
      |   (-a1)
      |   ((-a1))
      |   f(-a1)
      |   f((-a1))
      | }
    """.stripMargin) { res =>
    val expected = List(
      callSite(1, "p/o.A#`unary_-`().", "unary_-", implicitArgumentVal("p/o.a.")),
      callSite(2, "p/o.A#`unary_-`().", "unary_-", implicitArgumentVal("p/o.a.")),
      callSite(3, "p/o.A#`unary_-`().", "unary_-", implicitArgumentVal("p/o.a.")),
      callSite(
        4,
        "p/o.f().",
        "f[scala/Int#]",
        typeArgument("scala/Int#"),
        implicitArgumentVal("p/o.i.")),
      callSite(5, "p/o.A#`unary_-`().", "unary_-", implicitArgumentVal("p/o.a.")),
      callSite(
        6,
        "p/o.f().",
        "f[scala/Int#]",
        typeArgument("scala/Int#"),
        implicitArgumentVal("p/o.i.")),
      callSite(7, "p/o.A#`unary_-`().", "unary_-", implicitArgumentVal("p/o.a."))
    )

    checkElementsSorted(res.callSites, expected)

    res.callSitesCount shouldBe 9
  }

  callSites(
    "Infix methods",
    """
      | package p
      | object o {
      |   List(1) ++ List(2)
      | }
    """.stripMargin) { res =>
    val expected = List(
      callSite(
        callSiteId = 1,
        declarationId = "scala/collection/immutable/List.canBuildFrom().",
        code = "canBuildFrom[scala/Int#]",
        typeArgument("scala/Int#"),
        parentCallSite(2)
      ),
      callSite(
        callSiteId = 2,
        declarationId = "scala/collection/immutable/List#`++`().",
        code = "++",
        implicitArgumentCall(1)
      )
    )

    checkElementsSorted(res.callSites, expected)

    res.callSitesCount shouldBe 3
  }

  callSites(
    "Infix methods with parens",
    """
      | package p
      | object o {
      |   (List(1) ++ List(2)) :+ List(3)
      | }
    """.stripMargin) { res =>
    val expected = List(
      callSite(
        callSiteId = 1,
        declarationId = "scala/collection/immutable/List.canBuildFrom().",
        code = "canBuildFrom[scala/Any#]",
        typeArgument("scala/Any#"),
        parentCallSite(2)
      ),
      callSite(
        callSiteId = 2,
        declarationId = "scala/collection/SeqLike#`:+`().",
        code = ":+",
        implicitArgumentCall(1)
      ),
      callSite(
        callSiteId = 3,
        declarationId = "scala/collection/immutable/List.canBuildFrom().",
        code = "canBuildFrom[scala/Int#]",
        typeArgument("scala/Int#"),
        parentCallSite(4)
      ),
      callSite(
        callSiteId = 4,
        declarationId = "scala/collection/immutable/List#`++`().",
        code = "++",
        implicitArgumentCall(3)
      )
    )

    checkElementsSorted(res.callSites, expected)

    res.callSitesCount shouldBe 5
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

    res.callSitesCount shouldBe 2
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

    res.callSitesCount shouldBe 3
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

    res.callSitesCount shouldBe 3
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

    res.callSitesCount shouldBe 3
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

    // it is not for since the (row, col) does not count
    res.callSitesCount shouldBe 3
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

    res.callSitesCount shouldBe 4
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

    res.callSitesCount shouldBe 1
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

    res.callSitesCount shouldBe 2
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

    res.callSitesCount shouldBe 3
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

    res.callSitesCount shouldBe 2
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

    res.callSitesCount shouldBe 7
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

    res.callSitesCount shouldBe 3
  }

  // essentially: implicit conversion in implicit parameters
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

    res.callSitesCount shouldBe 4
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

    res.callSitesCount shouldBe 3
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

    res.callSitesCount shouldBe 4
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

    res.callSitesCount shouldBe 3
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

    res.callSitesCount shouldBe 2
  }

  callSites(
    "for-comprehension with filter",
    """
      | object o {
      |   for {
      |     i <- 1 to 10 if i % 2 == 0 // 1 (1 to 10),2 (i%2), 3 (.. == 0)
      |     j <- 2 until i // 4
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

    res.callSitesCount shouldBe 4
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

    res.callSitesCount shouldBe 3
  }

  callSites(
    "implicit argument in a constructor",
    """
      | package p
      | object o {
      |   class A[T](x: T)(implicit e: T)
      |
      |   implicit val i = 1
      |
      |   new A(1)
      | }
    """.stripMargin
  ) { res =>
    val expected = callSite(
      declarationId = "p/o.A#`<init>`().",
      code = "<init>",
      implicitArgumentVal("p/o.i.")
    )

    checkElementsSorted(res.callSites, List(expected))

    res.callSitesCount shouldBe 1
  }

  callSites(
    "implicit argument in a constructor in a new anonymous",
    """
      | package p
      | object o {
      |   class A[T](x: T)(implicit e: T)
      |
      |   implicit val i = 1
      |
      |   new A(1) { }
      | }
    """.stripMargin
  ) { res =>
    val expected = callSite(
      declarationId = "p/o.A#`<init>`().",
      code = "<init>",
      implicitArgumentVal("p/o.i.")
    )

    checkElementsSorted(res.callSites, List(expected))

    res.callSitesCount shouldBe 1
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

    res.callSitesCount shouldBe 1
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
    res.callSitesCount shouldBe 1
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
    res.callSitesCount shouldBe 1
  }

  callSites(
    "string interpolation with map",
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
        code = "map[scala/Int#, scala/Any#]",
        typeArgument("scala/Int#"),
        typeArgument("scala/Any#"), // TODO: why is it Any?
        implicitArgumentCall(1)
      )
    )

    checkElementsSorted(res.callSites, expected)

    res.callSitesCount shouldBe 4
  }

  callSites(
    "auxiliary constructors with implicits",
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

    res.callSitesCount shouldBe 5
  }

  callSites(
    "overwritten methods",
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

    res.callSitesCount shouldBe 3
  }

  callSites(
    "default parameter values with implicit call",
    """
      | package p
      | object o {
      |   class A
      |   implicit def a: A = new A
      |
      |   def f[T](x: T)(implicit y: Numeric[T], a: A) = y.abs(x)
      |
      |   def g[T](x: T = f(1))(implicit y: Numeric[T]) = 1
      |
      |   g(1)
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
        declarationId = "p/o.f().",
        code = "f[scala/Int#]",
        typeArgument("scala/Int#"),
        implicitArgumentVal("scala/math/Numeric.IntIsIntegral."),
        implicitArgumentCall(1)
      ),
      callSite(
        callSiteId = 3,
        declarationId = "p/o.g().",
        code = "g[scala/Int#]",
        typeArgument("scala/Int#"),
        implicitArgumentVal("scala/math/Numeric.IntIsIntegral.")
      )
    )

    checkElementsSorted(res.callSites, expected)

    res.callSitesCount shouldBe 4
  }

  callSites(
    "early initialization",
    """
      | package p
      | object o {
      |   def f[T](x: T)(implicit y: Numeric[T]): T = y.abs(x)
      |   trait T[A] {
      |     val z: A
      |   }
      |   new {
      |     val z = f(1)
      |   } with T[Int]
      | }
    """.stripMargin) { res =>
    val expected =
      callSite(
        declarationId = "p/o.f().",
        code = "f[scala/Int#]",
        typeArgument("scala/Int#"),
        implicitArgumentVal("scala/math/Numeric.IntIsIntegral.")
      )

    checkElementsSorted(res.callSites, List(expected))

    res.callSitesCount shouldBe 3
  }

  callSites(
    "Check if we are not missing some synthetics type parameters (cf. #30)",
    """
      | package p
      | object o {
      |   trait T[X] {
      |     def f(x: X) = 1
      |   }
      |   class A
      |
      |   implicit val ta = new T[A] {}
      |   implicit def tseq[X](implicit x: T[X]): T[Seq[X]] = new T[Seq[X]] {}
      |
      |   def f[X](x: X)(implicit y: T[X]) = y.f(x)
      |
      |   f(Seq(new A))
      | }
    """.stripMargin) { res =>
    val expected = List(
      callSite(
        callSiteId = 1,
        declarationId = "p/o.tseq().",
        code = "tseq",
        parentCallSite(2),
        // one would expect here typeArgument of [A]
        implicitArgumentVal("p/o.ta.")
      ),
      callSite(
        callSiteId = 2,
        declarationId = "p/o.f().",
        code = "f[scala/collection/Seq#[p/o.A#]]",
        typeArgument("scala/collection/Seq#", typeRef("p/o.A#")),
        implicitArgumentCall(1)
      )
    )

    checkElementsSorted(res.callSites, expected)

    res.callSitesCount shouldBe 6

    // the injected implicit to call to f: Synthetic(Some(Range(14,3,14,16)),ApplyTree(OriginalTree(Some(Range(14,3,14,16))),Vector(ApplyTree(IdTree(p/o.tseq().),Vector(IdTree(p/o.ta.))))))
    // the type parameter to f: Synthetic(Some(Range(14,3,14,4)),TypeApplyTree(OriginalTree(Some(Range(14,3,14,4))),Vector(TypeRef(Empty,scala/collection/Seq#,Vector(TypeRef(Empty,p/o.A#,Vector()))))))
    // the call to .apply in f(Seq(...)): Synthetic(Some(Range(14,5,14,8)),TypeApplyTree(SelectTree(OriginalTree(Some(Range(14,5,14,8))),Some(IdTree(scala/collection/generic/GenericCompanion#apply().))),Vector(TypeRef(Empty,p/o.A#,Vector()))))
    res.db.synthetics should have size 3
  }

  callSites(
    "local implicits",
    """
      | package p
      | object o {
      |   def f1(implicit x: String) = 1
      |   def f2(implicit x: String) = 1
      |   def g(implicit x: Int) = 1
      |   def test(block: Unit) = block
      |
      |   test {
      |     implicit val s1: String = "A"
      |     implicit def i1: Int = 1
      |     f1
      |     f2
      |     g
      |   }
      | }
    """.stripMargin) { implicit res =>
    val expected = List(
      callSite(
        callSiteId = 1,
        declarationId = "p/o.f1().",
        code = "f1",
        implicitArgumentVal("local0::1")),
      callSite(
        callSiteId = 2,
        declarationId = "p/o.f2().",
        code = "f2",
        implicitArgumentVal("local0::1")),
      callSite(
        callSiteId = 3,
        declarationId = "local1::2",
        code = "i1",
        parentCallSite(4)),
      callSite(
        callSiteId = 4,
        declarationId = "p/o.g().",
        code = "g",
        implicitArgumentCall(3))
    )

    checkElementsSorted(res.callSites, expected)

    res.callSitesCount shouldBe 4
  }

  callSites("string interpolation",
    """
      | package p
      | object o {
      |   class A
      |   class B
      |   implicit def b: B = new B
      |
      |   implicit class AHelper(val sc: StringContext) extends AnyVal {
      |     def a(args: Any*)(implicit b: B): A = new A
      |   }
      |
      |   def consume(a: A) = 1
      |
      |   consume(a"some")
      | }
    """.stripMargin) { res =>
    val expected = List(
      callSite(
        callSiteId = 1,
        declarationId = "p/o.b().",
        code = "b",
        parentCallSite(2)
      ),
      callSite(
        callSiteId = 2,
        declarationId = "p/o.AHelper#a().",
        code = "a",
        implicitArgumentCall(1)
      ),
      callSite(
        callSiteId = 3,
        declarationId = "p/o.AHelper().",
        code = "AHelper(a\"some\")"
      )
    )

    checkElementsSorted(res.callSites, expected)
    res.callSitesCount shouldBe 4
  }

  callSites(
    "assign call",
    """
      | package p
      | object o {
      |   class A
      |   implicit def a: A = new A
      |
      |   class B {
      |     private var _p: Int = 0
      |     def p = _p
      |     def p_=(x: Int)(implicit a: A) {
      |       this._p = x
      |     }
      |   }
      |
      |   val b = new B
      |   b.p = 1
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
        declarationId = "p/o.B#`p_=`().",
        code = "p_=",
        implicitArgumentCall(1)
      )
    )

    checkElementsSorted(res.callSites, expected)
    res.callSitesCount shouldBe 4
  }

  callSites(
    "update",
    """
      | package p
      | object o {
      |   class A
      |   implicit def a: A = new A
      |
      |   class B {
      |     def update(x: Int, y: String)(implicit a: A) = 1
      |   }
      |
      |   val b = new B
      |   b(1) = "A"
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
        declarationId = "p/o.B#update().",
        code = "update",
        implicitArgumentCall(1)
      )
    )

    checkElementsSorted(res.callSites, expected)
    res.callSitesCount shouldBe 3
  }

  callSites(
    "this call with implicits",
    """
      | package p
      | object o {
      |   class A
      |
      |   class B {
      |     implicit def a: A = new A
      |
      |     def apply(x: Int)(implicit a: A) =  1
      |
      |     def f = this(1)
      |   }
      | }
    """.stripMargin) { res =>
    val expected = List(
      callSite(
        callSiteId = 1,
        declarationId = "p/o.B#a().",
        code = "a",
        parentCallSite(2)
      ),
      callSite(
        callSiteId = 2,
        declarationId = "p/o.B#apply().",
        code = ".apply",
        implicitArgumentCall(1)
      )
    )

    checkElementsSorted(res.callSites, expected)
    res.callSitesCount shouldBe 2
  }

//  // IGNORED till #42 is resolved
//  callSites(
//    "implicit constructor parameters in extends in nested class",
//    """
//      | package p
//      | object o {
//      |   class A
//      |   implicit val a = new A
//      |
//      |   object n {
//      |     class B(x: Int)(implicit a: A)
//      |   }
//      |
//      |   class C(y: String)(implicit a: A) extends n.B(1)
//      |
//      |   new C("A")
//      | }
//    """.stripMargin) { res =>
//    val expected = List(
//      callSite(
//        callSiteId = 1,
//        declarationId = "p/o.n.B#`<init>`().",
//        code = "<init>",
//        implicitArgumentVal("p/o.C#`<init>`().(a)")
//      ),
//      callSite(
//        callSiteId = 2,
//        declarationId = "p/o.C#`<init>`().",
//        code = "<init>",
//        implicitArgumentVal("p/o.a.")
//      )
//    )
//
//    checkElementsSorted(res.callSites, expected)
//    res.callSitesCount shouldBe 2
//  }

  callSites(
    "implicit constructor parameters in extends",
    """
      | package p
      | object o {
      |   class A
      |   implicit val a = new A
      |
      |   class B(x: Int)(implicit a: A)
      |   class C(y: String)(implicit a: A) extends B(1)
      |
      |   new C("A")
      | }
    """.stripMargin) { res =>
    val expected = List(
      callSite(
        callSiteId = 1,
        declarationId = "p/o.B#`<init>`().",
        code = "<init>",
        implicitArgumentVal("p/o.C#`<init>`().(a)")
      ),
      callSite(
        callSiteId = 2,
        declarationId = "p/o.C#`<init>`().",
        code = "<init>",
        implicitArgumentVal("p/o.a.")
      )
    )

    checkElementsSorted(res.callSites, expected)
    res.callSitesCount shouldBe 2
  }

  callSites("implicit conversion with implicit parameters",
    """
      | package p
      | object o {
      |   implicit val s = "A"
      |   implicit def f(x: Int)(implicit y: String) = true
      |
      |   val b: Boolean = 1
      | }
    """.stripMargin) { res =>
    val expected = callSite(
      declarationId = "p/o.f().",
      code = "f(1)",
      implicitArgumentVal("p/o.s.")
    )

    checkElementsSorted(res.callSites, List(expected))

    res.callSitesCount shouldBe 0
  }

  callSites("implicit conversion in a companion object",
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
    """.stripMargin) { res =>
    // the `.apply().` indicates that it is from a companion object
    val expected =
      callSite(
        declarationId = "p/o.URL.apply().",
        code = "apply(\"http://fit.cvut.cz\")",
      )

    checkElementsSorted(res.callSites, List(expected))
    res.callSitesCount shouldBe 2
  }
}
