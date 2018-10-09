package cz.cvut.fit.prl.scala.implicits

import java.net.URLClassLoader

import better.files._
import cz.cvut.fit.prl.scala.implicits.utils.{BuildInfo, Libraries}
import org.scalatest.FunSuite
import java.io.{File => JFile}

import scala.compat.Platform.EOL
import scala.meta.interactive.InteractiveSemanticdb
import scala.meta.internal.semanticdb.scalac.SemanticdbPlugin
import scala.meta.internal.symtab.GlobalSymbolTable
import scala.meta.internal.{semanticdb => s}
import scala.meta._
import scala.meta.io.Classpath
import scala.tools.nsc.interactive.Global

abstract class SemanticdbSuite extends FunSuite {

  protected val classpath: Classpath = {
    val projectClasspath =
      Classpath(
      BuildInfo
        .test_externalDependencyClasspath
        .map(_.getAbsolutePath)
        .mkString(java.io.File.pathSeparator)
    )

    Libraries.JvmBootClasspath ++ projectClasspath
  }

  protected lazy val symtab: GlobalSymbolTable = GlobalSymbolTable(classpath)

  protected lazy val compiler: Global = {
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

  private def test(code: String)(fn: => Unit): Unit = {
    var name = code.trim.replace(EOL, " ")
    if (name.length > 50) name = name.take(50) + "..."
    super.test(name)(fn)
  }

  def database(code: String)(fn: s.TextDocument => Unit): Unit = {
    val db = computeDatabaseFromSnippet(code)
    test(code)(fn(db))
  }
}
