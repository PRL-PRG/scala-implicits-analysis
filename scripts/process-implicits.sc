import $ivy.`cz.cvut.fit.prl.scala.implicits::metadata:1.0-SNAPSHOT`
import $ivy.`cz.cvut.fit.prl.scala.implicits::model:1.0-SNAPSHOT`
import $ivy.`cz.cvut.fit.prl.scala.implicits::transformation:1.0-SNAPSHOT`

import better.files._
import cz.cvut.fit.prl.scala.implicits._
import cz.cvut.fit.prl.scala.implicits.model._

def run(implicit idx: Index): Unit = {

}

@main
def main(path: String = "../corpora/top100/") = {
  val corpusPath = File(path)

  println("Using build: " + utils.BuildInfo.buildInfoBuildNumber)
  println("Using corpus: " + corpusPath)

  val index: Index = Index(corpusPath)
  run(index)
}
