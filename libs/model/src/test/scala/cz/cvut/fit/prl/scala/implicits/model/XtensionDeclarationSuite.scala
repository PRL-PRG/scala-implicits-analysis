package cz.cvut.fit.prl.scala.implicits.model

import org.scalatest.{FunSuite, Matchers}

class XtensionDeclarationSuite extends FunSuite with Matchers with ModelDSL {

  test("companion implicit class def") {
    val defd = methodDeclaration(
      "A().",
      isImplicit,
      parameters(parameter("x", typeRef("scala/Int#"))),
      returnType("A#")
    )

    val implClazz = classDeclaration("A#", isImplicit)

    implicit val resolver: DeclarationResolver = DeclarationResolverStub(defd, implClazz)

    defd.isImplicitClassCompanionDef shouldBe true
    defd.implicitClassCompanion shouldBe Some(implClazz)
  }

}
