import $ivy.`cz.cvut.fit.prl.scala.implicits::metadata:1.0-SNAPSHOT`
import $ivy.`cz.cvut.fit.prl.scala.implicits::model:1.0-SNAPSHOT`
import $ivy.`cz.cvut.fit.prl.scala.implicits::tools:1.0-SNAPSHOT`
import cz.cvut.fit.prl.scala.implicits._
import cz.cvut.fit.prl.scala.implicits.model._
import cz.cvut.fit.prl.scala.implicits.model.Util._

import $ivy.`com.nrinaudo::kantan.csv-generic:0.4.0`
import kantan.csv._
import kantan.csv.ops._
import kantan.csv.generic._

import $ivy.`com.github.pathikrit::better-files:3.4.0`
import better.files._

def loadImplicits(filename: String = "implicits.bin"): ProjectIndex = {
    val p = File(filename).inputStream.apply(Project.read(_)).get
    ProjectIndex(p)
}