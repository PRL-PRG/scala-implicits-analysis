package cz.cvut.fit.prl.scala.implicits.symtab
import cz.cvut.fit.prl.scala.implicits.model.External
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

    inside(symbol.value.location) {
      case External(true, path, entry) =>
        path should include("scala-library")
        entry should be("scala/Int.class")
    }
  }

  test("Resolves project name") {
    val tab = GlobalSymbolTable(FullClasspath)
    val symbol =
      tab.resolve("cz/cvut/fit/prl/scala/implicits/symtab/SymbolTable#")

    inside(symbol.value.location) {
      case External(false, path, entry) =>
        path should endWith("classes")
        entry should endWith(classOf[SymbolTable].getSimpleName + ".class")
    }
  }
}
