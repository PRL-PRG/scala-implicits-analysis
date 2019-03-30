package cz.cvut.fit.prl.scala.implicits.tools

import better.files.File
import cz.cvut.fit.prl.scala.implicits.model._
import cz.cvut.fit.prl.scala.implicits.model.Declaration.Kind._

import scala.util.{Failure, Success, Try}

object ImplicitConversionExporter extends Exporter[ImplicitConversion] {

  private def encode(declaration: Declaration, from: TypeRef, to: TypeRef)(
      implicit idx: Index): ImplicitConversion = {
    ImplicitConversion(declaration, from, to)
  }

  def conversionTypes(d: Declaration)(implicit idx: Index): Option[(TypeRef, TypeRef)] = {
    if (!d.isImplicit) {
      None
    } else {
      implicit val module: Module = d.module
      d.kind match {
        case DEF =>
          val to = d.returnType.get
          ImplicitConversionParameters.unapply(d.parameterLists) match {
            case Some((Parameter(_, from, _), _)) if !to.isKindOf(declarationIds.Unit) =>
              // has non-unit return type and compatible parameters
              Some((from, to))
            case _ =>
              None
          }
        case VAL | VAR | OBJECT if d.hasMethodSignature =>
          val rt = d.returnType.get
          val f1 = (rt #:: rt.allParents).dropWhile(x => !x.isTypeOf(declarationIds.Function1))
          f1 match {
            case TypeRef(_, Seq(from, to)) #:: _ if !to.isKindOf(declarationIds.Unit) =>
              // type is of scala.Function1[A,B] where B is not scala.Unit
              Some((from, to))
            case _ =>
              None
          }
        case _ =>
          None
      }
    }
  }

  def export(d: Declaration)(implicit idx: Index): Option[Try[ImplicitConversion]] = {
    if (d.isProjectLocal || d.isImplicitClassCompanionDef) {
      // in case of implicit class, the actual conversion is not declared as project local, it is synthetic
      Try(conversionTypes(d)) match {
        case Success(Some((from, to))) => Some(Try(encode(d, from, to)))
        case Success(None) => None
        case Failure(e) => Some(Failure(e))
      }
    } else {
      None
    }
  }

  override def export(project: Project): Stream[Try[ImplicitConversion]] = {
    implicit val idx: Index = ProjectIndex(project)

    idx.implicitDeclarations
      .toStream
      .map(export)
      .collect {
        case Some(v) => v
      }
  }
}

import ImplicitConversion.implicits._

object ImplicitConversionExporterApp
    extends CSVExporterApp[ImplicitConversion](
      ImplicitConversionExporter,
      File("implicit-conversions.csv"))
