package cz.cvut.fit.prl.scala.implicits.tools.neo4j

import cz.cvut.fit.prl.scala.implicits.model.{ClasspathEntry, Declaration, Language, Location, Module, Parameter, ParameterList, PathEntry, SourcepathEntry, TypeRef}
import org.scalatest.{FunSuite, Matchers}


class ModelLogicSuite extends FunSuite with Matchers{

  test("getGroupArtifactSourcePath"){
    val path = "path"
    val groupId = "groupId"
    val artifactId = "artifactId"
    val pathEntry = SourcepathEntry(path, "", managed = false)
    val paths: Map[String, PathEntry] = Map(path -> pathEntry )
    val module = Module(null, null, groupId, artifactId, null, null, null, paths, null, null, 1, 1)

    val declaration = Declaration("", "", Declaration.Kind.CLASS,0, "", Declaration.Access.PRIVATE, Location(path, "", None), Language.SCALA, Seq(), null)

    ModelLogic.getGroupArtifact(declaration)(module) shouldEqual (groupId, artifactId)
  }

  test("getGroupArtifactClassPath"){
    val path = "path"
    val groupId = "groupId"
    val artifactId = "artifactId"
    val pathEntry = ClasspathEntry(path, groupId, artifactId, "", "", internal = false, managed = true, transitive = false)
    val paths: Map[String, PathEntry] = Map(path -> pathEntry )
    val module = Module(null, null, groupId + "other", artifactId + "other", null, null, null, paths, null, null, 1, 1)

    val declaration: Declaration = Declaration("", "", Declaration.Kind.CLASS,0, "", Declaration.Access.PRIVATE, Location(path, "", None), Language.SCALA, Seq(), null)

    ModelLogic.getGroupArtifact(declaration)(module) shouldEqual (groupId, artifactId)
  }

  test("getTypeExpression1"){
    val innerTypeRef0 = TypeRef("decl20", Seq())
    val innerTypeRef1 = TypeRef("decl10",Seq(innerTypeRef0))
    val innerTypeRef2 = TypeRef("decl11",Seq())

    val typeRef = TypeRef("decl00", Seq(innerTypeRef1, innerTypeRef2))

    ModelLogic.getTypeExpression(typeRef) shouldEqual "decl00[decl10[decl20],decl11]"
  }

  test("getTypeExpression2"){
    val typeRef = TypeRef("decl00", Seq())

    ModelLogic.getTypeExpression(typeRef) shouldEqual "decl00"
  }

  test("isImplicitPositive1"){
    val declaration = Declaration("", "", Declaration.Kind.CLASS, 32, "", Declaration.Access.PRIVATE, null, Language.SCALA, Seq(), null)

    ModelLogic.isImplicit(declaration) shouldBe true
  }

  test("isImplicitPositive2"){
    val declaration = Declaration("", "", Declaration.Kind.CLASS, 36, "", Declaration.Access.PRIVATE, null, Language.SCALA, Seq(), null)

    ModelLogic.isImplicit(declaration) shouldBe true
  }

  test("isImplicitNegative1"){
    val declaration = Declaration("", "", Declaration.Kind.CLASS, 0, "", Declaration.Access.PRIVATE, null, Language.SCALA, Seq(), null)

    ModelLogic.isImplicit(declaration) shouldBe false
  }

  test("isImplicitNegative2"){
    val declaration = Declaration("", "", Declaration.Kind.CLASS, 64, "", Declaration.Access.PRIVATE, null, Language.SCALA, Seq(), null)

    ModelLogic.isImplicit(declaration) shouldBe false
  }

  test("implicitConversionFunctionPositive1"){
    val typeRefFrom = TypeRef("declarationIdFrom")
    val typeRefTo = TypeRef("declarationIdTo")

    val typeRef = TypeRef(ModelLogic.FUNCTION1_DECLARATIONID, Seq(typeRefFrom, typeRefTo))

    ModelLogic.isImplicitConvFunction(typeRef) shouldBe true
  }

  test("implicitConversionFunctionNegative1BadFuncDecl"){
    val typeRefFrom = TypeRef("declarationIdFrom")
    val typeRefTo = TypeRef("declarationIdTo")

    val typeRef = TypeRef(ModelLogic.FUNCTION1_DECLARATIONID + "some", Seq(typeRefFrom, typeRefTo))

    ModelLogic.isImplicitConvFunction(typeRef) shouldBe false
  }

  test("implicitConversionFunctionNegative2notTwoArgs"){
    val typeRefFrom = TypeRef("declarationIdFrom")
    val typeRefTo = TypeRef("declarationIdTo")

    val typeRef = TypeRef(ModelLogic.FUNCTION1_DECLARATIONID, Seq(typeRefFrom, typeRefTo, typeRefTo))

    ModelLogic.isImplicitConvFunction(typeRef) shouldBe false
  }

  test("implicitConversionFunctionNegative3UnitFrom"){
    val typeRefFrom = TypeRef(ModelLogic.UNIT_DECLARATIONID)
    val typeRefTo = TypeRef("declarationIdTo")

    val typeRef = TypeRef(ModelLogic.FUNCTION1_DECLARATIONID, Seq(typeRefFrom, typeRefTo))

    ModelLogic.isImplicitConvFunction(typeRef) shouldBe false
  }

  test("implicitConversionFunctionNegative4UnitTo"){
    val typeRefFrom = TypeRef("declarationIdFrom")
    val typeRefTo = TypeRef(ModelLogic.UNIT_DECLARATIONID)

    val typeRef = TypeRef(ModelLogic.FUNCTION1_DECLARATIONID, Seq(typeRefFrom, typeRefTo))

    ModelLogic.isImplicitConvFunction(typeRef) shouldBe false
  }

  test("implicitConversionMethodPositive1"){
    val typeRefFrom = TypeRef("declarationIdFrom")
    val typeRefTo = TypeRef("declarationIdTo")

    val parameterLists = Seq(ParameterList(Seq(Parameter("paramName", typeRefFrom, isImplicit = false))))
    val returnType = typeRefTo

    ModelLogic.isImplicitConvMethod(parameterLists, returnType) shouldBe true
  }

  test("implicitConversionMethodPositive2"){
    val typeRefFrom = TypeRef("declarationIdFrom")
    val typeRefTo = TypeRef("declarationIdTo")
    val otherTypeRef = TypeRef("declarationIdOther")

    val firstParamList = ParameterList(Seq(Parameter("paramName", typeRefFrom, isImplicit = false)))
    val secondParamList = ParameterList(Seq(Parameter("paramName2", otherTypeRef, isImplicit = true)))
    val parameterLists = Seq(firstParamList, secondParamList)
    val returnType = typeRefTo

    ModelLogic.isImplicitConvMethod(parameterLists, returnType) shouldBe true
  }

  test("implicitConversionMethodNegative1"){
    val typeRefFrom = TypeRef("declarationIdFrom")
    val typeRefTo = TypeRef("declarationIdTo")
    val otherTypeRef = TypeRef("declarationIdOther")

    val firstParamList = ParameterList(Seq(Parameter("paramName", typeRefFrom, isImplicit = false)))
    val secondParamList = ParameterList(Seq(Parameter("paramName2", otherTypeRef, isImplicit = false)))
    val parameterLists = Seq(firstParamList, secondParamList)
    val returnType = typeRefTo

    ModelLogic.isImplicitConvMethod(parameterLists, returnType) shouldBe false
  }

  test("implicitConversionMethodNegative2"){
    val typeRefTo = TypeRef("declarationIdTo")

    val parameterLists = Seq()
    val returnType = typeRefTo

    ModelLogic.isImplicitConvMethod(parameterLists, returnType) shouldBe false
  }

  test("implicitConversionMethodNegative3"){
    val typeRefFrom = TypeRef("declarationIdFrom")
    val typeRefTo = TypeRef(ModelLogic.UNIT_DECLARATIONID)
    val otherTypeRef = TypeRef("declarationIdOther")

    val firstParamList = ParameterList(Seq(Parameter("paramName", typeRefFrom, isImplicit = false)))
    val secondParamList = ParameterList(Seq(Parameter("paramName2", otherTypeRef, isImplicit = false)))
    val parameterLists = Seq(firstParamList, secondParamList)
    val returnType = typeRefTo

    ModelLogic.isImplicitConvMethod(parameterLists, returnType) shouldBe false
  }

  test("implicitConversionMethodNegative4"){
    val typeRefFrom = TypeRef(ModelLogic.UNIT_DECLARATIONID)
    val typeRefTo = TypeRef("declarationIdTo")
    val otherTypeRef = TypeRef("declarationIdOther")

    val firstParamList = ParameterList(Seq(Parameter("paramName", typeRefFrom, isImplicit = false)))
    val secondParamList = ParameterList(Seq(Parameter("paramName2", otherTypeRef, isImplicit = true)))
    val parameterLists = Seq(firstParamList, secondParamList)
    val returnType = typeRefTo

    ModelLogic.isImplicitConvMethod(parameterLists, returnType) shouldBe false
  }

  test("getConversionTypes1"){
    val typeRefFrom = TypeRef("declarationIdFrom")
    val typeRefTo = TypeRef("declarationIdTo")
    val otherTypeRef = TypeRef("declarationIdOther")

    val firstParamList = ParameterList(Seq(Parameter("paramName", typeRefFrom, isImplicit = false)))
    val secondParamList = ParameterList(Seq(Parameter("paramName2", otherTypeRef, isImplicit = true)))
    val parameterLists = Seq(firstParamList, secondParamList)
    val returnType = typeRefTo

    ModelLogic.getConversionTypes(parameterLists, returnType) shouldEqual (typeRefFrom, typeRefTo)
  }

  test("getConversionTypes2"){
    val typeRefFrom = TypeRef("declarationIdFrom")
    val typeRefTo = TypeRef("declarationIdTo")

    val typeRef = TypeRef(ModelLogic.FUNCTION1_DECLARATIONID, Seq(typeRefFrom, typeRefTo))

    ModelLogic.getConversionTypes(typeRef) shouldEqual (typeRefFrom, typeRefTo)
  }
}
