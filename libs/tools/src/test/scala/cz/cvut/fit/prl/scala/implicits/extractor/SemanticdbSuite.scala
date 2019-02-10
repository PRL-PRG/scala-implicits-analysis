package cz.cvut.fit.prl.scala.implicits.extractor

import java.io.{File => JFile}
import java.net.URLClassLoader

import better.files._
import cz.cvut.fit.prl.scala.implicits.utils.{BuildInfo, Libraries}
import org.scalatest.FunSuite

import scala.meta.internal.semanticdb.scalac.SemanticdbPlugin
import scala.meta.internal.{semanticdb => s}
import scala.meta.io.Classpath
import scala.tools.nsc.{Global, Settings}
import scala.tools.nsc.reporters.StoreReporter
import scala.util.{Failure, Success, Try}

abstract class SemanticdbSuite extends FunSuite {

  val classpath: Classpath = {
    val projectClasspath =
      Classpath(
        BuildInfo.test_externalDependencyClasspath
          .map(_.getAbsolutePath)
          .mkString(java.io.File.pathSeparator)
      )

    Libraries.JvmBootClasspath ++ projectClasspath
  }

  lazy val pluginClasspath: String = classOf[SemanticdbPlugin].getClassLoader match {
    case cl: URLClassLoader => cl.getURLs.map(_.getFile).mkString(JFile.pathSeparator)
    case cl => sys.error(s"unsupported classloader: $cl")
  }

  val semanticdbOptions = List(
    "-P:semanticdb:text:on",
    "-P:semanticdb:symbols:all",
    "-P:semanticdb:synthetics:on",
    "-P:semanticdb:diagnostics:off",
    "-P:semanticdb:failures:warning"
  )

  val scalacOptions: List[String] =
    List(
      "-Xplugin:" + pluginClasspath,
      "-Xplugin-require:semanticdb",
      "-Yrangepos",
      "-classpath",
      classpath.entries.mkString(JFile.pathSeparator)
    )

  def computeDatabaseFromSnippet(code: String): (File, s.TextDocument) = {
    val output = File.newTemporaryDirectory("semanticdb-suite").deleteOnExit()
    val file = File.newTemporaryFile("code-", ".scala", Some(output))
    file.overwrite(code)

    compile(file)

    val sdbFile = output / "META-INF" / "semanticdb" / (file.name + ".semanticdb")
    val sdbs = sdbFile.inputStream.apply(s.TextDocuments.parseFrom)
    val sdb = sdbs.documents.head

    (output, sdb)
  }

  def compile(file: File): Unit = {
    val output = file.parent.pathAsString
    val options =
      semanticdbOptions ++ scalacOptions ++ List("-P:semanticdb:sourceroot:" + output, "-d", output)

    val settings = new Settings
    settings.processArgumentString(options.mkString(" "))

    val reporter = new StoreReporter
    val compiler = new Global(settings, reporter)
    val run = new compiler.Run()

    run.compile(List(file.pathAsString))

    if (reporter.hasErrors) {
      throw new Exception(
        s"Unable to compile $file",
        new Exception(reporter.infos.map(x => s"${x.severity} ${x.pos} ${x.msg}").mkString("\n\n"))
      )
    }
  }

  def database(name: String, code: String)(fn: (File, s.TextDocument) => Unit): Unit = {
    Try(computeDatabaseFromSnippet(code)) match {
      case Success((ouputDir, db)) => test(name)(fn(ouputDir, db))
      case Failure(e) => throw new Exception(s"Unable to generate semanticdb for `$name'", e)
    }
  }

  private def test(name: String)(fn: => Unit): Unit = {
    super.test(name)(fn)
  }
}
