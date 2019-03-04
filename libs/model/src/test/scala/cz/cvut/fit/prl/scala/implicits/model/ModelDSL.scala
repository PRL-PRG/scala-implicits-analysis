package cz.cvut.fit.prl.scala.implicits.model

import cz.cvut.fit.prl.scala.implicits.model.Declaration.Kind.{CLASS, DEF}
import cz.cvut.fit.prl.scala.implicits.model.Language.SCALA
import scalapb.lenses.{Lens, Mutation}

import scala.meta.internal.semanticdb.Scala._

object ModelDSL {
  val TestLocalLocation = Location("test-location", "", Some(Position(0, 0, 0, 0)))
  val TestExternalLocation = Location("test-external-location", "", None)
  val TestModuleId = "test-module"
  val TestCallSiteId: Int = 1
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

  def typeRef(ref: String, typeArguments: TypeRef*): TypeRef =
    TypeRef(ref, typeArguments.toList)

  def returnType(ref: String, typeArguments: TypeRef*): Update[Declaration] =
    _.method.returnType := typeRef(ref, typeArguments: _*)

  def implicitArgumentVal(ref: String): Update[CallSite] =
    _.implicitArgumentTypes.modify(_ :+ ValueRef(ref))

  def implicitArgumentCall(callSiteId: Int): Update[CallSite] =
    _.implicitArgumentTypes.modify(_ :+ CallSiteRef(callSiteId))

  def typeArgument(ref: String, typeArguments: TypeRef*): Update[CallSite] =
    _.typeArguments.modify(_ :+ typeRef(ref, typeArguments:_*))

  def parentCallSite(id: Int): Update[CallSite] =
    _.parentId := id

  def method(declarationId: String, updates: Update[Declaration]*): Declaration = {
    val d = Declaration(
      declarationId = declarationId,
      moduleId = TestModuleId,
      DEF,
      declarationId.desc.name.value,
      TestLocalLocation,
      SCALA,
      false,
      MethodSignature(returnType = typeRef("scala/Unit#")))

    d.update(updates: _*)
  }

  def clazz(declarationId: String, updates: Update[Declaration]*): Declaration = {
    val d =
      Declaration(
        declarationId = declarationId,
        moduleId = TestModuleId,
        CLASS,
        declarationId.desc.name.value,
        TestLocalLocation,
        SCALA,
        false
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
}
