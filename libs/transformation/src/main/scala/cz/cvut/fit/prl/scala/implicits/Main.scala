package cz.cvut.fit.prl.scala.implicits
import better.files._
import com.typesafe.scalalogging.LazyLogging
import cz.cvut.fit.prl.scala.implicits.extractor._
import cz.cvut.fit.prl.scala.implicits.model.Project
import cz.cvut.fit.prl.scala.implicits.utils._

object Main extends App with LazyLogging {

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
      name = metadata.versionEntries.head.projectId,
      declarations = declarations
    )

    val resultFile = projectPath / "_analysis_" / "implicits.bin"
    resultFile.outputStream.apply(project.writeTo)

    case class Failure(cause: Throwable, uri: String, symbol: String) {}

    if (exceptions.nonEmpty) {
      val failures = exceptions.collect { case x: ConversionException => x }

      failures.foreach { x =>
        println(x.longSummary)
        x.getCause.printStackTrace(System.out)
        println
      }

      println("Failure summary:")
      val summaries = failures.map(_.summary)
      summaries
        .groupBy(x => x)
        .mapValues(x => x.size)
        .toSeq
        .sortBy(-_._2)
        .foreach {
          case (problem, size) =>
            println(s"\t$size -- $problem")
        }
    }

    println
    println(s"Extracted ${declarations.size}/${declarations.size + exceptions.size} declarations into $resultFile.")
  }
  run(projectPath)
}
