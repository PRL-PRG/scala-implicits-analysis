package cz.cvut.fit.prl.scala.implicits.model

import org.scalatest.{FunSuite, Matchers}

class XtensionDeclarationSuite extends FunSuite with Matchers with ModelDSL {

  test("companion implicit class def") {
    val defd = method(
      "A().",
      isImplicit,
      parameters(parameter("x", "scala/Int#")),
      returnType("A#")
    )

    val implClazz = clazz("A#", isImplicit)

    implicit val resolver: TypeResolver = TypeResolverStub(defd, implClazz)

    defd.isImplicitClassCompanionDef shouldBe true
    defd.implicitClassCompanion shouldBe Some(implClazz)
  }

}
