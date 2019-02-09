import $ivy.`cz.cvut.fit.prl.scala.implicits::metadata:1.0-SNAPSHOT`
import $ivy.`cz.cvut.fit.prl.scala.implicits::model:1.0-SNAPSHOT`
import $ivy.`cz.cvut.fit.prl.scala.implicits::transformation:1.0-SNAPSHOT`
import $ivy.`com.nrinaudo::kantan.csv-generic:0.4.0`
import $ivy.`com.github.pathikrit::better-files:3.4.0`

import $file.ExportImplicitDeclarations

import cz.cvut.fit.prl.scala.implicits._
import cz.cvut.fit.prl.scala.implicits.model._

import better.files._

def run(implicit idx: Index, outputPath: File): Unit = {
  ExportImplicitDeclarations.export(idx, outputPath / "implicit-declarations.csv")
}

@main
def main(path: String) = {
  val corpusPath = File(path)

  println("Using build: " + utils.BuildInfo.buildInfoBuildNumber)
  println("Using corpus: " + corpusPath.path.toAbsolutePath)

  val index: Index = Index(corpusPath)
  run(index, corpusPath)
}
