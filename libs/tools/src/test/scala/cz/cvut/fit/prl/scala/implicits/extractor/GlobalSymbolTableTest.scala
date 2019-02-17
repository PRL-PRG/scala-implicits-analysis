package cz.cvut.fit.prl.scala.implicits.extractor
import cz.cvut.fit.prl.scala.implicits.utils.{BuildInfo, Libraries}
import org.scalatest.{FunSuite, Inside, Matchers, OptionValues}

import scala.meta.io.Classpath

class GlobalSymbolTableTest extends FunSuite with Matchers with OptionValues with Inside {

  val ExternalClasspath: Classpath = {
    val projectClasspath =
      Classpath(
        BuildInfo.test_externalDependencyClasspath
          .map(_.getAbsolutePath)
          .mkString(java.io.File.pathSeparator)
      )

    Libraries.JvmBootClasspath ++ projectClasspath
  }

  val FullClasspath: Classpath = {
    ExternalClasspath ++ Classpath(
      BuildInfo.productDirectories
        .map(_.getAbsolutePath)
        .mkString(java.io.File.pathSeparator)
    )
  }

  test("Resolves external name") {
    val tab = new GlobalSymbolTable(ExternalClasspath)
    val symbol = tab.resolve("scala/Int#")
    val location = symbol.value.location

    location.path should endWith("scala-library-2.12.7.jar")
    location.relativeUri should endWith("scala/Int.class")
    location.position should not be defined
  }

  test("Resolves project local name") {
    val tab = new GlobalSymbolTable(FullClasspath)
    val symbol = tab.resolve("cz/cvut/fit/prl/scala/implicits/extractor/SymbolTable#")
    val location = symbol.value.location

    location.path should be("tools/target/scala-2.12/classes")
    location.relativeUri should be("cz/cvut/fit/prl/scala/implicits/extractor/SymbolTable.class")
    location.position should not be defined
  }
}
