package cz.cvut.fit.prl.scala.implicits.tests

import org.scalatest.{FunSuite, Matchers}
import better.files._
import cz.cvut.fit.prl.scala.implicits.ProjectMetadata
import cz.cvut.fit.prl.scala.implicits.extractor.{DeclarationExtractor, ExtractionContext}
import cz.cvut.fit.prl.scala.implicits.model.{Index, ModelDSL}
import cz.cvut.fit.prl.scala.implicits.tools.ExtractImplicits
import cz.cvut.fit.prl.scala.implicits.utils._

class MetadataSuite extends FunSuite with Matchers {

  test("Project versions") {
    val (metadata, warnings) = ProjectMetadata(ProjectPath)
    warnings should have size 0
    metadata.modules should have size 2
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
    val ctx = new ExtractionContext(
      ModelDSL.TestModuleId,
      subProjectMetadata.resolver,
      metadata.sourcepathEntries.map(_.path))
    val extractor = new DeclarationExtractor(ctx)
    val db = subProjectMetadata.semanticdbs.find(_.uri.endsWith("JsonExample.scala")).get

    val (declarations, failures) = extractor.extractImplicitDeclarations(db).split()
    failures.foreach(_.printStackTrace())
    failures shouldBe empty

    declarations should not be empty
  }
}
