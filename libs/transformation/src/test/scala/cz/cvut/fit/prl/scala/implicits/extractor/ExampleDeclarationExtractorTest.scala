package cz.cvut.fit.prl.scala.implicits.extractor

import better.files._
import cz.cvut.fit.prl.scala.implicits.ProjectMetadata
import cz.cvut.fit.prl.scala.implicits.utils._
import org.scalatest.{FunSuite, Matchers}

class ExampleDeclarationExtractorTest extends FunSuite with Matchers {

  val ProjectPath = File("example")

  test("Project versions") {
    val (metadata, warnings) = ProjectMetadata(ProjectPath)
    warnings should have size 0
    metadata.modules should have size 1
    metadata.modules.head.classpathEntries.size should be > 1
  }

  test("Load project") {
    val (metadata, warnings) = ProjectMetadata(ProjectPath)
    metadata.modules.head.resolver.resolveSymbol("com/example/Example.")
    warnings should have size 0
  }

  test("JSON") {
    val (metadata, warnings) = ProjectMetadata(ProjectPath)
    warnings should have size 0
    val subProjectMetadata = metadata.modules.head
    val ctx = new ExtractionContext(subProjectMetadata.resolver)
    val extractor = new DeclarationExtractor(ctx)
    val db = subProjectMetadata.semanticdbs.find(_.uri.endsWith("JsonExample.scala")).get

    val (declarations, failures) = extractor.extractImplicitDeclarations(db).split()
    failures.foreach(_.printStackTrace())
    failures shouldBe empty

    declarations.prettyPrint()
  }
}
