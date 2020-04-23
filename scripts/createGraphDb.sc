import $ivy.`cz.cvut.fit.prl.scala.implicits::tools:1.0-SNAPSHOT`
import $ivy.`org.typelevel::kittens:1.2.0`

import better.files._
import cz.cvut.fit.prl.scala.implicits.tools.ImplicitsToNeo4j

@main
def main() = {
  println(File.currentWorkingDirectory)
  ExtractImplicits.run(File.currentWorkingDirectory, File(File.currentWorkingDirectory + "/_analysis_/implicits.bin"))
}