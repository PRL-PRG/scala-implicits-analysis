package cz.cvut.fit.prl.scala.implicits.model
import better.files.File
import org.scalatest.FunSuite

class IndexTest extends FunSuite {

  test("parsing examples") {
    val idx = Index(File("example/_analysis_/implicits.bin"))
    println(idx)
  }

//  test("parsing top100") {
//    val idx = Index(File("../corpora/4-top100/projects/scala-js--scala-js/_analysis_/implicits.bin"))
//    idx.moduleMap
//  }
//  test("parsing top100") {
//    val idx = Index(File("../corpora/4-top100/implicits.bin"))
//    idx.moduleMap
//  }

}
