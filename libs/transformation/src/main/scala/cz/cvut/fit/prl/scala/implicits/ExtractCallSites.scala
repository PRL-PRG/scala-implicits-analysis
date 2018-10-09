package cz.cvut.fit.prl.scala.implicits

import cz.cvut.fit.prl.scala.implicits.utils.Libraries

import scala.meta.internal.{semanticdb => s}
import java.nio.file.Path
import better.files._
import java.io.{File => JFile}

import kantan.csv._
import kantan.csv.ops._
import kantan.csv.generic._

import scala.meta.internal.semanticdb.Locator
import scala.meta.internal.symtab.GlobalSymbolTable
import scala.meta.io.{AbsolutePath, Classpath}

case class Project(
                    projectId: String,
                    path: Path,
                    classpath: Seq[Path]
                  )

case class ExtractedCallSites(
                               project: Project,
                               path: Path,
                               callSites: Seq[CallSite],
                               declarations: Seq[Declaration],
                               failures: Seq[Throwable]
                             ) {

}

case class ProjectClasspath(projectId: String, projectName: String, path: String)

object ExtractCallSites extends App {

  val path = File(args(0))
  val classpathFile = path / "_analysis_" / "classpath.csv"
  val classpathCSV = classpathFile.path.asUnsafeCsvReader[ProjectClasspath](rfc.withHeader).toList
  val classpath = classpathCSV.map(_.path).distinct.map(AbsolutePath(_))
  val symtab = GlobalSymbolTable(Libraries.JvmBootClasspath ++ Classpath(classpath))

  val results = collection.mutable.ArrayBuffer[ExtractedCallSites]()
  Locator(path.path) {
    case (path, dbs) => results += extract(path, dbs)
  }

  val result = results.head
  println(
    s"""
       |path: ${result.path}, failures: ${result.failures.size}, callsites: ${result.callSites.size}
       |${result.failures.mkString("\n\n")}
      """.stripMargin)


  def extract(path: Path, dbs: s.TextDocuments): ExtractedCallSites = {
    val extractors = dbs.documents.map(new CallSiteExtractor(_, symtab))

    val (callSites, declarations, failures) = extractors.unzip3(x => (x.callSites, x.declarations.values, x.failures))

    ExtractedCallSites(
      Project("", path, Seq()),
      path,
      callSites.flatten,
      declarations.flatten,
      failures.flatten
    )
  }

}
