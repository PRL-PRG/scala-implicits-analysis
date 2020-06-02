import $ivy.`cz.cvut.fit.prl.scala.implicits:tools_2.12:1.0-SNAPSHOT`
import $ivy.`org.typelevel:kittens_2.12:1.2.0`

import better.files._
import cz.cvut.fit.prl.scala.implicits.tools.ImplicitsToNeo4j

@main
def main(corporaDir: String = File.currentWorkingDirectory.toString, implicitsFileRelative: String = "/implicits-valid.bin") = {

//  println(File.currentWorkingDirectory)
  ImplicitsToNeo4j.run(File(corporaDir), File(corporaDir + implicitsFileRelative))
}
