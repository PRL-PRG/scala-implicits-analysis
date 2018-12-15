import $ivy.`cz.cvut.fit.prl.scala.implicits::transformation:1.0-SNAPSHOT`
import $ivy.`org.typelevel::kittens:1.2.0`

import better.files._
import cz.cvut.fit.prl.scala.implicits.tools.ExtractImplicits

@main
def main(source: String, outputDir: String) = {
  ExtractImplicits.run(source, File(outputDir))
}
