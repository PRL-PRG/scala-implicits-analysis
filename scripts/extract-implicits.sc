import $ivy.`cz.cvut.fit.prl.scala.implicits::transformation:1.0-SNAPSHOT`
import $ivy.`org.typelevel::cats-core:1.4.0`
import better.files._
import cz.cvut.fit.prl.scala.implicits.tools.ExtractImplicits

@main
def main(projectsFile: String) = {
  ExtractImplicits.run(File(projectsFile), File("implicits.bin"), File("implicits-exceptions.log"), System.getenv("N_JOBS").toInt)
}
