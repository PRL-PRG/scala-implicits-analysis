package cz.cvut.fit.prl.scala.implicits.model

import cz.cvut.fit.prl.scala.implicits.model.Declaration.Kind
import cz.cvut.fit.prl.scala.implicits.model.Declaration.Kind._
import cz.cvut.fit.prl.scala.implicits.model.Language.SCALA
import scalapb.lenses.{Lens, Mutation, ObjectLens}

import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.semanticdb.SymbolInformation.{Property => p}

object ModelDSL {
  val TestLocalLocation = Location("test-location", "", Some(Position(0, 0, 0, 0)))
  val TestExternalLocation = Location("test-external-location", "", None)
  val TestModuleId = "test-module"
  val TestProjectId = "test-project"
  val TestGroupId = "test-group"
  val TestArtifactId = "test-artifact"
  val TestVersion = "1.0"

  val TestCallSiteId: Int = 1
}

trait ModelDSL {
  import ModelDSL._

  type Update[A] = Lens[A, A] => Mutation[A]

  class OverloadHack1
  class OverloadHack2

  implicit val overloadHack1 = new OverloadHack1
  implicit val overloadHack2 = new OverloadHack2

  implicit class DeclarationLens(that: Lens[Declaration, Declaration]) extends ObjectLens[Declaration, Declaration](that) {
    def method: _root_.scalapb.lenses.Lens[Declaration, MethodSignature] =
      field(_.methodSignature)((c_, f_) => c_.copy(signature = f_))
    def `class`: _root_.scalapb.lenses.Lens[Declaration, ClassSignature] =
      field(_.classSignature)((c_, f_) => c_.copy(signature = f_))
    def `type`: _root_.scalapb.lenses.Lens[Declaration, TypeSignature] =
      field(_.typeSignature)((c_, f_) => c_.copy(signature = f_))
    def value: _root_.scalapb.lenses.Lens[Declaration, ValueSignature] =
      field(_.valueSignature)((c_, f_) => c_.copy(signature = f_))
  }

  def isImplicit: Update[Declaration] =
    _.properties.modify(_ | p.IMPLICIT.value)

  def parameters(v: Parameter*)(implicit ev: OverloadHack1): Update[Declaration] =
    _.method.parameterLists.modify(_ :+ ParameterList(v.toSeq))

  def parameter(name: String, tpe: Type, isImplicit: Boolean = false): Parameter =
    Parameter(name, tpe, isImplicit)

  def typeRef(ref: String, typeArguments: Type*): TypeRef =
    TypeRef(ref, typeArguments.toList)

  def tparamRef(ref: String, name: String): TypeParameterRef =
    TypeParameterRef(ref, name)

  def returnType(ref: String, typeArguments: TypeRef*): Update[Declaration] =
    _.method.returnType := typeRef(ref, typeArguments: _*)

  def returnType(tpe: Type): Update[Declaration] =
    _.method.returnType := tpe

  def implicitArgumentVal(ref: String): Update[CallSite] =
    _.implicitArgumentTypes.modify(_ :+ ValueRef(ref))

  def implicitArgumentCall(callSiteId: Int): Update[CallSite] =
    _.implicitArgumentTypes.modify(_ :+ CallSiteRef(callSiteId))

  def typeArgument(ref: String, typeArguments: TypeRef*): Update[CallSite] =
    _.typeArguments.modify(_ :+ typeRef(ref, typeArguments: _*))

  def parentCallSite(value: Int): Update[CallSite] =
    _.parentId := value

  def name(value: String): Update[Declaration] =
    _.name := value

  def properties(value: Int): Update[Declaration] =
    _.properties := value

  def access(value: Declaration.Access): Update[Declaration] =
    _.access := value

  def parent(ref: String, typeArguments: TypeRef*): Update[Declaration] =
    _.`class`.parents.modify(_ :+ typeRef(ref, typeArguments: _*))

  def typeParameter(
      name: String,
      upperBound: Type = Type.Empty,
      lowerBound: Type = Type.Empty): Update[Declaration] =
    _.method.typeParameters
      .modify(_ :+ TypeParameter(name, upperBound = upperBound, lowerBound = lowerBound))

  def lowerBound(ref: String, typeArguments: TypeRef*): Update[Declaration] =
    _.`type`.lowerBound := typeRef(ref, typeArguments: _*)

  def upperBound(ref: String, typeArguments: TypeRef*): Update[Declaration] =
    _.`type`.upperBound := typeRef(ref, typeArguments: _*)

  def methodDeclaration(declarationId: String, updates: Update[Declaration]*): Declaration = {
    declaration(declarationId, DEF, updates: _*)
  }

  def classDeclaration(declarationId: String, updates: Update[Declaration]*): Declaration = {
    declaration(declarationId, CLASS, updates: _*)
  }

  def typeDeclaration(declarationId: String, updates: Update[Declaration]*): Declaration = {
    declaration(declarationId, TYPE, updates: _*)
  }

  def objectDeclaration(declarationId: String, updates: Update[Declaration]*): Declaration = {
    declaration(declarationId, OBJECT, updates: _*)
  }

  def valueDeclaration(
      declarationId: String,
      tpe: Type,
      updates: Update[Declaration]*): Declaration = {
    declaration(declarationId, VAL, updates: _*).update(_.value.tpe := tpe)
  }

  def parameterDeclaration(
      declarationId: String,
      tpe: Type,
      updates: Update[Declaration]*): Declaration = {
    declaration(declarationId, PARAMETER, updates: _*).update(_.value.tpe := tpe)
  }

  def declaration(declarationId: String, kind: Kind, updates: Update[Declaration]*): Declaration = {
    val d =
      Declaration(
        declarationId = declarationId,
        moduleId = TestModuleId,
        kind = kind,
        properties = 0,
        declarationId.desc.name.value,
        Declaration.Access.PUBLIC,
        TestLocalLocation,
        SCALA,
        annotations = Seq.empty,
        kind match {
          case DEF =>
            MethodSignature(returnType = typeRef("scala/Unit#"))
          case TYPE =>
            TypeSignature()
          case CLASS | OBJECT =>
            ClassSignature()
          case VAL | VAR | PARAMETER =>
            ValueSignature(tpe = typeRef("scala/Unit#"))
        }
      )

    d.update(updates: _*)
  }

  def callSite(declarationId: String, code: String, updates: Update[CallSite]*): CallSite = {
    callSite(TestCallSiteId, declarationId, code, updates: _*)
  }

  def callSite(
      callSiteId: Int,
      declarationId: String,
      code: String,
      updates: Update[CallSite]*): CallSite = {
    val cs = CallSite(callSiteId, None, TestModuleId, declarationId, code, TestLocalLocation)
    cs.update(updates: _*)
  }

  implicit val callSiteOrdering: Ordering[CallSite] = (x: CallSite, y: CallSite) =>
    implicitly[Ordering[Int]].compare(x.callSiteId, y.callSiteId)
}
