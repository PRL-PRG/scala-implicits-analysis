package cz.cvut.fit.prl.scala.implicits.tools

import better.files.File
import cz.cvut.fit.prl.scala.implicits.model._
import cz.cvut.fit.prl.scala.implicits.model.Declaration.Kind._

import scala.util.{Failure, Success, Try}

object ImplicitParameterExporter extends Exporter[ImplicitParameter] {

  override def export(project: Project): Stream[Try[ImplicitParameter]] = {
    implicit val idx: ProjectIndex = ProjectIndex(project)

    // we have to do this since some of the declarations used in
    // implicit call sites are regular functions expanded from macros
    // and this won't be captured by the implicitDeclaration
    val all =
      idx.implicitDeclarations ++ idx.implicitCallSites.map(_.declaration)

    all.toStream
      .distinct
      .filter(_.hasImplicitParameters)
      .flatMap { decl =>
        decl.implicitParameterList
          .map(_.parameters.map(p => Try(ImplicitParameter(p, decl))))
          .getOrElse(Seq())
      }
  }
}

import ImplicitParameter.implicits.encoder

object ImplicitParameterExporterApp
    extends CSVExporterApp[ImplicitParameter](
      ImplicitParameterExporter,
      File("implicit-parameters.csv"))
