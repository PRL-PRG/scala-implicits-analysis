package cz.cvut.fit.prl.scala.implicits.model

import cz.cvut.fit.prl.scala.implicits.model.Declaration.Kind.{CLASS, DEF}
import cz.cvut.fit.prl.scala.implicits.model.Language.SCALA

import scalapb.lenses.{Lens, Mutation}
import scala.meta.internal.semanticdb.Scala._

object ModelDSL {
    val TestLocalLocation = Location("test-location", "", Some(Position(0, 0, 0, 0)))
    val TestExternalLocation = Location("test-external-location", "", None)
    val TestModuleId = "test-module"
  }

trait ModelDSL {
  import ModelDSL._

  type Update[A] = Lens[A, A] => Mutation[A]

  class OverloadHack1
  class OverloadHack2

  implicit val overloadHack1 = new OverloadHack1
  implicit val overloadHack2 = new OverloadHack2

  def isImplicit: Update[Declaration] =
    _.isImplicit := true

  def parameters(v: Parameter*)(implicit ev: OverloadHack1): Update[Declaration] =
    _.method.parameterLists.modify(_ :+ ParameterList(v.toSeq))

  def parameters(v: Parameter*)(implicit ev: OverloadHack2): Update[MethodSignature] =
    _.parameterLists.modify(_ :+ ParameterList(v.toSeq))

  def parameter(name: String, ref: String, isImplicit: Boolean = false): Parameter =
    Parameter(name, typeRef(ref), isImplicit)

  def typeRef(ref: String, typeArguments: List[TypeRef] = Nil): TypeRef =
    TypeRef(ref, typeArguments)

  def returnType(ref: String, typeArguments: List[TypeRef] = Nil): Update[Declaration] =
    _.method.returnType := typeRef(ref, typeArguments)

  def method(fqn: String, updates: Update[Declaration]*): Declaration = {
    val d = Declaration(
      TestModuleId,
      fqn,
      DEF,
      fqn.desc.name.value,
      TestLocalLocation,
      SCALA,
      false,
      MethodSignature(returnType = typeRef("scala/Unit#")))

    d.update(updates: _*)
  }

  def clazz(fqn: String, updates: Update[Declaration]*): Declaration = {
    val d =
      Declaration(TestModuleId, fqn, CLASS, fqn.desc.name.value, TestLocalLocation, SCALA, false)

    d.update(updates: _*)
  }
}
