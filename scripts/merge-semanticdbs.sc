import $ivy.`cz.cvut.fit.prl.scala.implicits::transformation:1.0-SNAPSHOT`

import better.files._

import cz.cvut.fit.prl.scala.implicits.tools.MergeSemanticdbs

@main
def main(projectsFile: String) = {
  MergeSemanticdbs.run(File(projectsFile), File("merged-semanticdbs.bin"))
}
