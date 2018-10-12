//package cz.cvut.fit.prl.scala.implicits.model
//
//import cats.syntax.either._
//import io.circe._
//import io.circe.generic.auto._
//import io.circe.yaml._
//import io.circe.yaml.syntax._
//import io.circe.parser._
//import io.circe.syntax._
//
//import org.scalatest.FunSuite
//
//class YamlSerializationTest extends FunSuite {
//
//  test("Yaml can serialize protobuf generated classes") {
//    val m = Project("project-name", declarations = Seq(Declaration("a.b.c", "c", Declaration.Kind.DEF, LibraryLocation("a-b.jar"))))
//    println(m)
//    println(m.asJson.asYaml.spaces2)
//  }
//
//}
