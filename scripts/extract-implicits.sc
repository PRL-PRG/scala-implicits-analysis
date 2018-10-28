import $ivy.`cz.cvut.fit.prl.scala.implicits::transformation:1.0-SNAPSHOT`
import better.files._
import cz.cvut.fit.prl.scala.implicits.tools.ExtractImplicits

@main
def main(projectsFile: String) = {
  ExtractImplicits.run(File(projectsFile), File("implicits.bin"), 64)
}
