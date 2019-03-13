import $ivy.`cz.cvut.fit.prl.scala.implicits::tools:1.0-SNAPSHOT`
import $ivy.`cz.cvut.fit.prl.scala.implicits::metadata:1.0-SNAPSHOT`
import $ivy.`org.typelevel::kittens:1.2.0`

import better.files._
import cz.cvut.fit.prl.scala.implicits.metadata.MetadataFilenames._
import cz.cvut.fit.prl.scala.implicits.model.Index
import cz.cvut.fit.prl.scala.implicits.tools.ExportImplicitCallSites
import cz.cvut.fit.prl.scala.implicits.tools.ExportImplicitDeclarations

@main
def main() = {
  val baseDir = File.currentWorkingDirectory
  val index = Index.fromProjectsFile(baseDir / ExtractedImplicitsFilename)

  ExportImplicitCallSites.run(index)
  ExportImplicitDeclarations.run(index)
}
