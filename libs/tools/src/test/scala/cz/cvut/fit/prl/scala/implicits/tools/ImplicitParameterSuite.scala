package cz.cvut.fit.prl.scala.implicits.tools

import cz.cvut.fit.prl.scala.implicits.extractor.{ExtractionContextSuite, ModelSimplification}
import cz.cvut.fit.prl.scala.implicits.model.ModelDSL

class ImplicitParameterSuite extends ExtractionContextSuite
  with ModelSimplification
  with ModelDSL {

  import ModelDSL._
  import ImplicitParameterExporter.export

  project(
    "@implicitNotFound",
    s"""
      | package p
      | import scala.annotation.implicitNotFound
      | object o {
      |   @implicitNotFound("T is missing")
      |   trait T[A]
      |
      |   type R = T[Int]
      |
      |   def f(x: Int)(implicit r: R) { }
      | }
    """.stripMargin) { implicit project =>
    val parameters = export(project).map(_.get).toList

    parameters.size shouldBe 1
    parameters.head.typeAnnotations.get should contain("scala/annotation/implicitNotFound#")
  }

  project(
    "context / type-class",
    """
      | package p
      | object o {
      |   trait Request[T]
      |
      |   def f1(x: Int)(implicit r1: Request[Int]) {
      |
      |   }
      |
      |   def f2[T](x: T)(implicit r2: Request[T]) {
      |
      |   }
      | }
    """.stripMargin) { implicit project =>
    val parameters = export(project).map(_.get).toList

    parameters.size shouldBe 2

    val r1 = parameters.find(_.name == "r1").get
    r1.numTypeArguments shouldBe 1
    r1.numTypeArgumentRefs shouldBe 0

    val r2 = parameters.find(_.name == "r2").get
    r2.numTypeArguments shouldBe 1
    r2.numTypeArgumentRefs shouldBe 1
  }

  project(
    "type class",
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
      |   def asJson[T: Jsonable](x: T) {
      |    implicitly[Jsonable[T]].toJson(x)
      |   }
      |
      |   Seq(1,2,3).toJson
      |   asJson(Seq(1,2,3))
      | }
    """.stripMargin) { implicit project =>
    val parameters = export(project).map(_.get).toList

    // there is also CanBuildFrom and implicitly
    parameters.size shouldBe 5
    parameters.count(_.defGroupId == TestGroupId) shouldBe 3
  }

  project(
    "type class explicitly",
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
      |   implicit class XtensionJson[T](x: T)(implicit ev: Jsonable[T]) {
      |     def toJson: String = ev.toJson(x)
      |   }
      |
      |   def asJson[T](x: T)(implicit ev: Jsonable[T]) {
      |    ev.toJson(x)
      |   }
      |
      |   Seq(1,2,3).toJson
      |   asJson(Seq(1,2,3))
      | }
    """.stripMargin) { implicit project =>
    val parameters = export(project).map(_.get).toList

    // there is also CanBuildFrom
    parameters.size shouldBe 4
    parameters.count(_.defGroupId == TestGroupId) shouldBe 3
  }

}
