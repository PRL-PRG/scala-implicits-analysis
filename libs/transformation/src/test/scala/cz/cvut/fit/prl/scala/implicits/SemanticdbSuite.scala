package cz.cvut.fit.prl.scala.implicits

import java.io.{File => JFile}
import java.net.URLClassLoader

import better.files._
import cz.cvut.fit.prl.scala.implicits.symtab.GlobalSymbolTable
import cz.cvut.fit.prl.scala.implicits.utils.{BuildInfo, Libraries}
import org.scalatest.FunSuite

import scala.meta.interactive.InteractiveSemanticdb
import scala.meta.internal.semanticdb.scalac.SemanticdbPlugin
import scala.meta.internal.{semanticdb => s}
import scala.meta.io.Classpath
import scala.tools.nsc.interactive.Global

abstract class SemanticdbSuite extends FunSuite {

  val classpath: Classpath = {
    val projectClasspath =
      Classpath(
      BuildInfo
        .test_externalDependencyClasspath
        .map(_.getAbsolutePath)
        .mkString(java.io.File.pathSeparator)
    )

    Libraries.JvmBootClasspath ++ projectClasspath
  }

  lazy val symtab: GlobalSymbolTable = GlobalSymbolTable(classpath)

  lazy val compiler: Global = {
    val pluginClasspath = classOf[SemanticdbPlugin].getClassLoader match {
      case cl: URLClassLoader => cl.getURLs.map(_.getFile).mkString(JFile.pathSeparator)
      case cl => sys.error(s"unsupported classloader: $cl")
    }

    val scalacOptions =
      List(
        "-Xplugin:" + pluginClasspath,
        "-Xplugin-require:semanticdb",
        "-Yrangepos",
        "-Ystop-after:semanticdb-typer"
      )

    val classpathAsString = classpath.entries.mkString(java.io.File.pathSeparator)

    InteractiveSemanticdb.newCompiler(classpathAsString, scalacOptions)
  }

  def computeDatabaseFromSnippet(code: String): s.TextDocument = {
    val options = List(
      "-P:semanticdb:text:on",
      "-P:semanticdb:symbols:all",
      "-P:semanticdb:synthetics:on",
      "-P:semanticdb:diagnostics:off",
      "-P:semanticdb:failures:warning"
    )

    val manifestDir = File.newTemporaryDirectory("test")
    val file = manifestDir / "code.scala"
    file.overwrite(code)

    InteractiveSemanticdb.toTextDocument(
      compiler, code, "semanticdb-test.scala", 10000, options
    )
  }

  def database(name: String, code: String)(fn: s.TextDocument => Unit): Unit = {
    val db = computeDatabaseFromSnippet(code)
    test(name)(fn(db))
  }

  private def test(name: String)(fn: => Unit): Unit = {
    super.test(name)(fn)
  }
}
