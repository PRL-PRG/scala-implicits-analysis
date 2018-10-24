package cz.cvut.fit.prl.scala.implicits

import better.files._
import cz.cvut.fit.prl.scala.implicits.extractor.{DeclarationExtractor, ExtractionContext}
import cz.cvut.fit.prl.scala.implicits.utils._
import org.scalatest.{FunSuite, Matchers}

class ExampleDeclarationExtractionTest extends FunSuite with Matchers {

  val ProjectPath = File("example")

  test("Project versions") {
    val metadata = new ProjectMetadata(ProjectPath)
    metadata.versionEntries should have size 1
  }

  test("Project class path") {
    val metadata = new ProjectMetadata(ProjectPath)
    metadata.classpathEntries.size should be > 1
  }

  test("Load project") {
    val metadata = new ProjectMetadata(ProjectPath)
    metadata.resolver.resolveSymbol("com/example/Example.")
  }

  test("JSON") {
    val metadata = new ProjectMetadata(ProjectPath)
    val ctx = new ExtractionContext(metadata.resolver)
    val extractor = new DeclarationExtractor(ctx)
    val db = metadata.semanticdbs.find(_.uri.endsWith("JsonExample.scala")).get

    val (declarations, failures) = extractor.extractImplicitDeclarations(db).split()
    failures.foreach(_.printStackTrace())
    failures shouldBe empty

    declarations.prettyPrint()
  }
}
