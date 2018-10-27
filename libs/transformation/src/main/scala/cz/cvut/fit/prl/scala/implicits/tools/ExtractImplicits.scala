package cz.cvut.fit.prl.scala.implicits.tools
import better.files.File
import com.typesafe.scalalogging.LazyLogging
import cz.cvut.fit.prl.scala.implicits.extractor.{
  DeclarationConversionException,
  DeclarationExtractor,
  ExtractionContext
}
import cz.cvut.fit.prl.scala.implicits.model.Project
import cz.cvut.fit.prl.scala.implicits.utils._
import cz.cvut.fit.prl.scala.implicits.{Constants, ProjectMetadata}

object ExtractImplicits extends App with LazyLogging {

  val projectPath = File(args(0))

  if (args.length != 1) {
    sys.exit(1)
  }

  def run(projectPath: File): Unit = {
    val metadata = new ProjectMetadata(projectPath)
    val ctx = new ExtractionContext(metadata.resolver)
    val extractor = new DeclarationExtractor(ctx)

    val (declarations, exceptions) =
      metadata.semanticdbs
        .flatMap(extractor.extractImplicitDeclarations)
        .split()

    val project = Project(
      projectId = metadata.projectId,
      declarations = declarations
    )

    val resultFile = projectPath / Constants.AnalysisDirname / Constants.ExtractedImplicitsFilename
    resultFile.outputStream.apply(project.writeTo)

    if (exceptions.nonEmpty) {
      val failures = exceptions.collect { case x: DeclarationConversionException => x }

      failures.foreach { x =>
        println(x.longSummary)
        x.getCause.printStackTrace(System.out)
        println
      }

      println("Failure summary:")
      failures.printGroups(_.summary)
    }

    println
    println(s"Extracted ${declarations.size}/${declarations.size + exceptions.size} declarations into $resultFile.")
  }

  run(projectPath)
}
