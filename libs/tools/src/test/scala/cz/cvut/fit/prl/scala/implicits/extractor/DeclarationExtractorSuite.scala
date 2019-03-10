package cz.cvut.fit.prl.scala.implicits.extractor

import cz.cvut.fit.prl.scala.implicits.model.Declaration.Kind._
import cz.cvut.fit.prl.scala.implicits.model.Language.{JAVA, SCALA}
import cz.cvut.fit.prl.scala.implicits.model._
import cz.cvut.fit.prl.scala.implicits.model.ModelDSL._

class DeclarationExtractorSuite
    extends ExtractionContextSuite
    with ModelSimplification
    with ModelDSL {

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
        name("i1"),
        isImplicit
      )
    )

    checkElement(
      res.declarationAt(8),
      objectDeclaration(
        declarationId = "local",
        name("i2"),
        isImplicit,
        parent("p/o.T#")
      )
    )

    checkElement(
      res.declarationAt(9),
      methodDeclaration(
        declarationId = "local",
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
        name("x"),
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
    res.declarations.apply(1)

    val decl = res.declarations.find(_.isMethod).get
    val param = decl.parameterDeclaration("that")

    param.language should be(JAVA)
    decl.returnType.get.language should be(SCALA)
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
      name("x"),
      isImplicit,
      returnType("p/o.C#")
    )

    checkElements(res.declarations, List(expected))
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
      "x",
      TestLocalLocation,
      SCALA,
      true,
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
      "x",
      TestLocalLocation,
      SCALA,
      true,
      Seq.empty,
      TypeSignature(
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
        d.returnType.get.name shouldBe "String"

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
        "XtensionJson",
        TestLocalLocation,
        SCALA,
        true,
        Seq.empty,
        TypeSignature(List(TypeParameter("T")))
      ),
      Declaration(
        "p/o.XtensionJson#ev.",
        TestModuleId,
        VAL,
        "ev",
        TestLocalLocation,
        SCALA,
        true,
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
        "XtensionJson",
        TestLocalLocation,
        SCALA,
        true,
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
        "evidence$1",
        TestLocalLocation,
        SCALA,
        true,
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
        "XtensionJson",
        TestLocalLocation,
        SCALA,
        true,
        Seq.empty,
        TypeSignature(List(TypeParameter("T")))
      ),
      Declaration(
        "p/o.XtensionJson().",
        TestModuleId,
        DEF,
        "XtensionJson",
        TestLocalLocation,
        SCALA,
        true,
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
