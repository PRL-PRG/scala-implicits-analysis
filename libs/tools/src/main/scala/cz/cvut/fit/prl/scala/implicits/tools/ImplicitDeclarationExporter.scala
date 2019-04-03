package cz.cvut.fit.prl.scala.implicits.tools

import better.files.File
import cz.cvut.fit.prl.scala.implicits.model._

import scala.util.Try

object ImplicitDeclarationExporter extends Exporter[ImplicitDeclaration] {

  def encode(declaration: Declaration)(implicit idx: Index): ImplicitDeclaration = {
   ImplicitDeclaration(declaration)
  }

  override def export(project: Project): Stream[Try[ImplicitDeclaration]] = {
    implicit val idx: ProjectIndex = ProjectIndex(project)

    // we have to do this since some of the declarations used in
    // implicit call sites are regular functions expanded from macros
    // and this won't be captured by the implicitDeclaration
    val all =
      idx.implicitDeclarations ++ idx.implicitCallSites.map(_.declaration)

    all.toStream
      .distinct
      .map { declaration =>
        Try(encode(declaration))
      }
  }
}

import ImplicitDeclaration.implicits.encoder

object ImplicitDeclarationExporterApp
    extends CSVExporterApp[ImplicitDeclaration](
      ImplicitDeclarationExporter,
      File("implicit-declarations.csv"))
