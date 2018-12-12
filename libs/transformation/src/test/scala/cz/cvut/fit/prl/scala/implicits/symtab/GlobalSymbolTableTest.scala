package cz.cvut.fit.prl.scala.implicits.symtab
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
    val tab = GlobalSymbolTable(ExternalClasspath)
    val symbol = tab.resolve("scala/Int#")

    symbol.value.location shouldBe defined

    val location = symbol.value.location.get

    location.uri should endWith("scala-library-2.12.7.jar!scala/Int.class")
    location.position should not be defined
  }

  test("Resolves project local name") {
    val tab = GlobalSymbolTable(FullClasspath)
    val symbol =
      tab.resolve("cz/cvut/fit/prl/scala/implicits/symtab/SymbolTable#")

    symbol.value.location shouldBe defined

    val location = symbol.value.location.get

    location.uri should endWith("target/scala-2.12/classes/cz/cvut/fit/prl/scala/implicits/symtab/SymbolTable.class")
    location.position should not be defined
  }
}
