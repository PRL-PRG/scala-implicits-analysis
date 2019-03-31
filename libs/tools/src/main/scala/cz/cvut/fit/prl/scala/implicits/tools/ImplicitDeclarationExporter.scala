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

    idx.implicitDeclarations
      .toStream
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
