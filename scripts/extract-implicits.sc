import $ivy.`cz.cvut.fit.prl.scala.implicits::tools:1.0-SNAPSHOT`
import $ivy.`cz.cvut.fit.prl.scala.implicits::metadata:1.0-SNAPSHOT`
import $ivy.`org.typelevel::kittens:1.2.0`

import better.files._
import cz.cvut.fit.prl.scala.implicits.tools.ExtractImplicits
import cz.cvut.fit.prl.scala.implicits.metadata.MetadataFilenames.AnalysisDirname

@main
def main() = {
  ExtractImplicits.run(File.currentWorkingDirectory, File(AnalysisDirname))
}
