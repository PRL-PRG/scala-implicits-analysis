package cz.cvut.fit.prl.scala.implicits.extractor

import cz.cvut.fit.prl.scala.implicits.model.Declaration.Access.{
  NOT_SPECIFIED,
  PRIVATE_THIS,
  PUBLIC
}
import cz.cvut.fit.prl.scala.implicits.model.Declaration.Kind._
import cz.cvut.fit.prl.scala.implicits.model.Language.{JAVA, SCALA}
import cz.cvut.fit.prl.scala.implicits.model._
import cz.cvut.fit.prl.scala.implicits.model.ModelDSL._

class DeclarationExtractorSuite
    extends ExtractionContextSuite
    with ModelSimplification
    with ModelDSL {

  declarations(
    "local type parameter",
    """
      | package p
      | object o {
      |   def f() {
      |     implicit def c[T](x: T): Seq[T] = Seq(x)
      |
      |     1.map(_ + 1)
      |   }
      | }
    """.stripMargin) { res =>
    val defc = res.originalDeclarations.head

    defc.signature.method.get.returnType shouldBe typeRef(
      "scala/package.Seq#",
      tparamRef(defc.declarationId, "T")
    )

    defc.parameterLists.head.parameters.head.tpe shouldBe tparamRef(defc.declarationId, "T")
  }

  declarations(
    "implicitNotFound annotation",
    """
      | package p
      | object o {
      |   @scala.annotation.implicitNotFound("Message")
      |   trait TC[T]
      |
      |   implicit def f[T](x: T)(implicit ev: TC[T]) = x
      | }
    """.stripMargin) { res =>
    res.declarations should have size 1

    val tcId = res.declarationAt(6).implicitParameterList.get.parameters.head.tpe.declarationId
    val tc = res.declaration(tcId)

    tc.annotations should contain only TypeRef("scala/annotation/implicitNotFound#")
  }

  declarations(
    "local implicit",
    """
      | package p
      | object o {
      |   trait T
      |   def g[T](x: T)(implicit y: T) = y
      |
      |   def f() = {
      |     implicit val i1 = 1
      |     implicit object i2 extends T
      |     implicit def i3 = "A"
      |
      |     Seq(true, false).foreach { implicit x =>
      |       g(x)
      |     }
      |   }
      | }
    """.stripMargin) { res =>
    checkElement(
      res.declarationAt(4),
      methodDeclaration(
        declarationId = "p/o.g().",
        properties(0),
        typeParameter("T"),
        parameters(
          parameter("x", tparamRef("p/o.g().", "T"))
        ),
        parameters(
          parameter("y", tparamRef("p/o.g().", "T"), isImplicit = true)
        ),
        returnType(tparamRef("p/o.g().", "T"))
      )
    )

    checkElement(
      res.declarationAt(7),
      valueDeclaration(
        declarationId = "local",
        tpe = typeRef("scala/Int#"),
        properties(1056),
        access(NOT_SPECIFIED),
        name("i1"),
        isImplicit
      )
    )

    checkElement(
      res.declarationAt(8),
      objectDeclaration(
        declarationId = "local",
        properties(40),
        name("i2"),
        isImplicit,
        parent("p/o.T#")
      )
    )

    checkElement(
      res.declarationAt(9),
      methodDeclaration(
        declarationId = "local",
        properties(32),
        name("i3"),
        isImplicit,
        returnType("java/lang/String#")
      )
    )

    checkElement(
      res.declarationAt(11),
      parameterDeclaration(
        declarationId = "local",
        tpe = typeRef("scala/Boolean#"),
        properties(32),
        name("x"),
        access(NOT_SPECIFIED),
        isImplicit
      )
    )

    res.declarations should have size 5
  }

  declarations(
    "identify java/scala symbol",
    """
      | package p
      | object o {
      |   implicit class C(that: java.io.File) {
      |     def x = 1
      |   }
      |
      |   new java.io.File("").x
      | }
    """.stripMargin) { implicit res =>
    val decl = res.declarations.find(_.isMethod).get
    val param = decl.parameterDeclaration("that")

    param.language should be(JAVA)
    decl.returnDeclaration.get.language should be(SCALA)
  }

  declarations(
    "java reference",
    """
      | package p
      | object o {
      |   implicit def x: java.util.List[String] = ???
      | }
    """.stripMargin) { implicit res =>
    val expected = methodDeclaration(
      declarationId = "p/o.x().",
      properties(32),
      name("x"),
      isImplicit,
      returnType("java/util/List#", typeRef("scala/Predef.String#"))
    )

    checkElements(res.declarations, List(expected))
  }

  declarations(
    "this.type",
    """
      | package p
      | object o {
      |   class C[T] {
      |     implicit def x: this.type = ???
      |   }
      | }
    """.stripMargin) { implicit res =>
    val expected = methodDeclaration(
      declarationId = "p/o.C#x().",
      properties(32),
      name("x"),
      isImplicit,
      returnType("p/o.C#")
    )

    checkElements(res.declarations, List(expected))
  }

  declarations(
    "type definition",
    """
      | package p
      | object o {
      |   trait T {
      |     type X
      |     implicit def f(x: X): Int = 1
      |   }
      |
      |   class A
      |   class B extends A
      |
      |   type T1 = A
      |   type T2 >: B
      |   type T3 <: A
      |
      |   implicit def f1(x: T1): Int = 1
      |   implicit def f2(x: T2): Int = 1
      |   implicit def f3(x: T3): Int = 1
      | }
    """.stripMargin) { implicit res =>
    checkElement(
      res.declaration("p/o.T1#").simplify(res.ctx),
      typeDeclaration(
        declarationId = "p/o.T1#",
        upperBound("p/o.A#"),
        lowerBound("p/o.A#")
      )
    )

    checkElement(
      res.declaration("p/o.T2#").simplify(res.ctx),
      typeDeclaration(
        declarationId = "p/o.T2#",
        properties(4),
        lowerBound("p/o.B#")
      )
    )

    checkElement(
      res.declaration("p/o.T3#").simplify(res.ctx),
      typeDeclaration(
        declarationId = "p/o.T3#",
        properties(4),
        upperBound("p/o.A#")
      )
    )

    checkElement(
      res.declaration("p/o.T#X#").simplify(res.ctx),
      typeDeclaration(
        declarationId = "p/o.T#X#",
        properties(4)
      )
    )
  }

  // per https://github.com/lampepfl/dotty/pull/2060#issuecomment-284859761
  declarations("compilation unit - simulacrum case",
    """
      | package p
      | trait Semigroup[A] {
      |   def append(x: A, y: A): A
      | }
      |
      | object Semigroup {
      |   def apply[A](implicit instance: Semigroup[A]): Semigroup[A] = instance
      |
      |   trait Ops[A] {
      |     def typeClassInstance: Semigroup[A]
      |     def self: A
      |     def |+|(y: A): A = typeClassInstance.append(self, y)
      |   }
      |
      |   trait ToSemigroupOps {
      |     implicit def toSemigroupOps[A](target: A)(implicit tc: Semigroup[A]): Ops[A] = new Ops[A] {
      |       val self = target
      |       val typeClassInstance = tc
      |     }
      |   }
      | }
    """.stripMargin) { implicit res =>
    // the Ops and the implicit conversion toSemigroupOps are in the same compilation group
    val toSemigroupOps = res.declaration("p/Semigroup.ToSemigroupOps#toSemigroupOps().").compilationUnit
    val Ops = res.declaration("p/Semigroup.Ops#").compilationUnit

    toSemigroupOps.get shouldBe "p/Semigroup."
    Ops.get shouldBe "p/Semigroup."
  }

  declarations(
    "type definition model returnDeclaration",
    """
      | package p
      | object o {
      |   type T1 = Int
      |   type T2 = T1
      |   type T3 = T2
      |
      |   implicit def f(x: T3): Int = 1
      | }
    """.stripMargin) { implicit res =>
    res.declaration("p/o.f().").returnDeclaration.get.declarationId shouldBe "scala/Int#"
  }

  declarations(
    "implicit var",
    """
      | package p
      | object o {
      |   implicit var x = 1
      | }
    """.stripMargin) { implicit res =>
    val expected = Declaration(
      "p/o.x().",
      TestModuleId,
      VAR,
      2080,
      "x",
      PUBLIC,
      TestLocalLocation,
      SCALA,
      Seq.empty,
      MethodSignature(
        returnType = TypeRef("scala/Int#")
      )
    )

    checkElements(res.declarations, Seq(expected))
  }

  declarations(
    "implicit object",
    """
      | package p
      | object o {
      |   trait T
      |   implicit object x extends T
      | }
    """.stripMargin) { implicit res =>
    val expected = Declaration(
      "p/o.x.",
      TestModuleId,
      OBJECT,
      40,
      "x",
      PUBLIC,
      TestLocalLocation,
      SCALA,
      Seq.empty,
      ClassSignature(
        parents = Vector(
          TypeRef("p/o.T#")
        )
      )
    )

    checkElements(res.declarations, Seq(expected))
  }

  declarations(
    "implicit def",
    """
      | package p
      | object o {
      |   implicit def i2s(that: Int): String = that.toString
      | }
    """.stripMargin) { implicit res =>
    res.declarations should have size 1

    // this iis super ugly, but I keep it here just for reference
    inside(res.declarations.head) {
      case d =>
        d.kind shouldBe DEF
        d.name shouldBe "i2s"
        d.isImplicit shouldBe true
        d.location shouldBe TestLocalLocation
        d.language.isScala shouldBe true
        d.returnDeclaration.get.name shouldBe "String"

        d.parameterLists should have size 1
        inside(d.parameterLists.head) {
          case pl =>
            pl.isImplicit shouldBe false
            pl.parameters should have size 1

            inside(pl.parameters.head) {
              case p =>
                p.isImplicit shouldBe false
                p.name shouldBe "that"
                p.tpe.declarationId shouldBe "scala/Int#"
            }
        }
    }
  }

  declarations(
    "type class with explicit evidence",
    """
      | package p
      | object o {
      |   trait Jsonable[T] {
      |     def toJson(x: T): String = x.toString
      |   }
      |
      |   implicit class XtensionJson[T](x: T)(implicit ev: Jsonable[T]) {
      |     def toJson: String = ev.toJson(x)
      |   }
      | }
    """.stripMargin
  ) { implicit res =>
    val expected = List(
      Declaration(
        "p/o.XtensionJson#",
        TestModuleId,
        CLASS,
        32,
        "XtensionJson",
        PUBLIC,
        TestLocalLocation,
        SCALA,
        Seq.empty,
        ClassSignature(List(TypeParameter("T")))
      ),
      Declaration(
        "p/o.XtensionJson#ev.",
        TestModuleId,
        VAL,
        1056,
        "ev",
        PRIVATE_THIS,
        TestLocalLocation,
        SCALA,
        Seq.empty,
        MethodSignature(
          returnType = TypeRef(
            "p/o.Jsonable#",
            List(
              TypeParameterRef(
                "p/o.XtensionJson#",
                "T"
              )
            )
          )
        )
      ),
      Declaration(
        "p/o.XtensionJson().",
        TestModuleId,
        DEF,
        32,
        "XtensionJson",
        PUBLIC,
        TestLocalLocation,
        SCALA,
        Seq.empty,
        MethodSignature(
          List(TypeParameter("T")),
          List(
            ParameterList(
              List(
                Parameter(
                  "x",
                  TypeParameterRef(
                    "p/o.XtensionJson().",
                    "T"
                  ),
                  false
                )
              )
            ),
            ParameterList(
              List(
                Parameter(
                  "ev",
                  TypeRef(
                    "p/o.Jsonable#",
                    List(
                      TypeParameterRef(
                        "p/o.XtensionJson().",
                        "T"
                      )
                    )
                  ),
                  true
                )
              )
            )
          ),
          TypeRef(
            "p/o.XtensionJson#",
            List(
              TypeParameterRef(
                "p/o.XtensionJson().",
                "T"
              )
            )
          )
        )
      )
    )

    checkElements(res.declarations, expected)
  }

  declarations(
    "type class with implicit evidence",
    """
      | package p
      | object o {
      |   trait Jsonable[T] {
      |     def toJson(x: T): String = x.toString
      |   }
      |
      |   implicit class XtensionJson[T: Jsonable](x: T) {
      |     def toJson: String = implicitly[Jsonable[T]].toJson(x)
      |   }
      | }
    """.stripMargin
  ) { implicit res =>
    val expected = List(
      Declaration(
        "p/o.XtensionJson#evidence$1.",
        TestModuleId,
        VAL,
        1056,
        "evidence$1",
        PRIVATE_THIS,
        TestLocalLocation,
        SCALA,
        Seq.empty,
        MethodSignature(
          returnType = TypeRef(
            "p/o.Jsonable#",
            List(
              TypeParameterRef(
                "p/o.XtensionJson#",
                "T"
              )
            )
          )
        )
      ),
      Declaration(
        "p/o.XtensionJson#",
        TestModuleId,
        CLASS,
        32,
        "XtensionJson",
        PUBLIC,
        TestLocalLocation,
        SCALA,
        Seq.empty,
        ClassSignature(List(TypeParameter("T")))
      ),
      Declaration(
        "p/o.XtensionJson().",
        TestModuleId,
        DEF,
        32,
        "XtensionJson",
        PUBLIC,
        TestLocalLocation,
        SCALA,
        Seq.empty,
        MethodSignature(
          List(TypeParameter("T")),
          List(
            ParameterList(
              List(
                Parameter(
                  "x",
                  TypeParameterRef(
                    "p/o.XtensionJson().",
                    "T"
                  ),
                  false
                )
              )
            ),
            ParameterList(
              List(
                Parameter(
                  "evidence$1",
                  TypeRef(
                    "p/o.Jsonable#",
                    List(
                      TypeParameterRef(
                        "p/o.XtensionJson().",
                        "T"
                      )
                    )
                  ),
                  true
                )
              )
            )
          ),
          TypeRef(
            "p/o.XtensionJson#",
            List(
              TypeParameterRef(
                "p/o.XtensionJson().",
                "T"
              )
            )
          )
        )
      )
    )

    checkElements(expected, res.declarations)
  }

}
