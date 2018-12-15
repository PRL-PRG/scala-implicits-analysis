package cz.cvut.fit.prl.scala.implicits

import cz.cvut.fit.prl.scala.implicits.model.Declaration.Kind._
import cz.cvut.fit.prl.scala.implicits.model.Language.SCALA
import cz.cvut.fit.prl.scala.implicits.model._

class DeclarationExtractorSuite extends ExtractionContextSuite {

  declarations(
    "java reference",
    """
      | package p
      | object o {
      |   implicit def x: java.util.List[String] = ???
      | }
    """.stripMargin) { implicit res =>
    val expected = Declaration(
      DEF,
      "p/o.x().",
      "x",
      Some(TestLocalLocation),
      SCALA,
      true,
      MethodSignature(
        returnType = TypeRef("java/util/List#", List(TypeRef("scala/Predef.String#", List())))
      )
    )

    checkElements(res.declarations, Seq(expected))
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
    val expected = Declaration(
      DEF,
      "p/o.C#x().",
      "x",
      Some(TestLocalLocation),
      SCALA,
      true,
      MethodSignature(
        returnType = TypeRef("p/o.C#")
      )
    )

    checkElements(res.declarations, Seq(expected))
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
      VAR,
      "p/o.x().",
      "x",
      Some(TestLocalLocation),
      SCALA,
      true,
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
      OBJECT,
      "p/o.x.",
      "x",
      Some(TestLocalLocation),
      SCALA,
      true,
      TypeSignature(
        parents = List(
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
        d.location shouldBe Some(TestLocalLocation)
        d.language.isScala shouldBe true
        d.returnType.name shouldBe "String"

        d.parameterLists should have size 1
        inside(d.parameterLists.head) {
          case pl =>
            pl.isImplicit shouldBe false
            pl.parameters should have size 1

            inside(pl.parameters.head) {
              case p =>
                p.isImplicit shouldBe false
                p.name shouldBe "that"
                p.tpe.declaration.name shouldBe "Int"
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
        CLASS,
        "p/o.XtensionJson#",
        "XtensionJson",
        Some(TestLocalLocation),
        SCALA,
        true,
        TypeSignature(List(TypeParameter("T")))
      ),
      Declaration(
        VAL,
        "p/o.XtensionJson#ev.",
        "ev",
        Some(TestLocalLocation),
        SCALA,
        true,
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
        DEF,
        "p/o.XtensionJson().",
        "XtensionJson",
        Some(TestLocalLocation),
        SCALA,
        true,
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
        VAL,
        "p/o.XtensionJson#evidence$1.",
        "evidence$1",
        Some(TestLocalLocation),
        SCALA,
        true,
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
        CLASS,
        "p/o.XtensionJson#",
        "XtensionJson",
        Some(TestLocalLocation),
        SCALA,
        true,
        TypeSignature(List(TypeParameter("T")))
      ),
      Declaration(
        DEF,
        "p/o.XtensionJson().",
        "XtensionJson",
        Some(TestLocalLocation),
        SCALA,
        true,
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
                "T",
              )
            )
          )
        )
      )
    )

    checkElement(expected, res.declarations)
  }

}
