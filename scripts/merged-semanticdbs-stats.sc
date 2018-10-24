import $ivy.`cz.cvut.fit.prl.scala.implicits::transformation:1.0-SNAPSHOT`

import better.files._

import cz.cvut.fit.prl.scala.implicits.tools.MergedSemanticdbsStats

@main
def main(projectsFile: String) = {
  MergedSemanticdbsStats.run(File(projectsFile))
}

