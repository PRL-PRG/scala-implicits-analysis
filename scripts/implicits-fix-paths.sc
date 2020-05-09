import $ivy.`cz.cvut.fit.prl.scala.implicits:tools_2.12:1.0-SNAPSHOT`
import $ivy.`org.typelevel:kittens_2.12:1.2.0`

import better.files._
import cz.cvut.fit.prl.scala.implicits.tools.ImplicitsPathAdjuster

@main
def main(corporaDir: String = File.currentWorkingDirectory.toString, implicitsFileRelative: String = "/implicits.bin") = {
	val implicitsFile = File(corporaDir + implicitsFileRelative)
	val adjustedImplicitsFile = File(corporaDir + "/implicits2.bin")
  ImplicitsPathAdjuster(implicitsFile, adjustedImplicitsFile)
}
