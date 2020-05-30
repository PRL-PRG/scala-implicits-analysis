package cz.cvut.fit.prl.scala.implicits.tools.neo4j

import cz.cvut.fit.prl.scala.implicits.model.{ClasspathEntry, Declaration, Module, ParameterList, SourcepathEntry, TypeRef}

object ModelLogic {

  val UNIT_DECLARATIONID = "scala/Unit#"
  val FUNCTION1_DECLARATIONID = "scala/Function1#"

  def getGroupArtifact(declaration: Declaration)(implicit module: Module): (String, String) = {
    // relative path should be already adjusted to match paths in module - by script which validates the implicits.bin input
    val path = declaration.location.path

    // For Debugging purpuses, when the paths do not match
    //    val entryPathOpt = module.paths.get(path)
    //    if (entryPathOpt.isEmpty) {
    //      println("Adjusted path")
    //      debug(path)
    //      println("Raw path")
    //      debug(declaration.location.path)
    //      debugPaths(path, module.paths.keys)
    //    }

    val entryPath = module.paths(path)

    entryPath match {
      case ClasspathEntry(_, groupId, artifactId, _, _, _, _, _) =>
        (groupId, artifactId)
      case SourcepathEntry(_, _, _) =>
        (module.groupId, module.artifactId)
      case _ => throw new IllegalArgumentException("No path \"" + path + "\" found! ")
    }
  }


  // Declaration is not unique in the whole scope - groupId and artifactId could be added to ensure the uniqueness
  // but it is good enough for most purposes and is more performing
  def getTypeExpression(typeArgument: TypeRef): String = {
    val declarationId = typeArgument.declarationId
    if (typeArgument.typeArguments.isEmpty) declarationId
    else {
      val typeArguments = typeArgument.typeArguments.map(getTypeExpression).mkString("[",",","]")
      declarationId + typeArguments
    }
  }

  def isImplicit(declaration: Declaration): Boolean = (declaration.properties & 0x20) != 0

  def isImplicitConvFunction(returnType: TypeRef): Boolean = {
    // is function A => B, where A,B is non-unit type
    if (returnType.declarationId != FUNCTION1_DECLARATIONID ||
      returnType.typeArguments.size != 2) {
      false
    }
    else {
      val fromTypeArg = returnType.typeArguments.head
      val toTypeArg = returnType.typeArguments.tail.head

      if (fromTypeArg.declarationId == UNIT_DECLARATIONID || toTypeArg.declarationId == UNIT_DECLARATIONID)
        false
      else {
        true
      }
    }
  }

  def isImplicitConvMethod(parameterLists: Seq[ParameterList], returnType: TypeRef): Boolean = {
    // is function A=>B with exactly one non-implicit parameter in the first parameter list
    // and zero or more implicit parameters in second parameter list
    if (returnType.declarationId == UNIT_DECLARATIONID)
      return false

    if (parameterLists.isEmpty || parameterLists.head.parameters.size != 1)
      return false

    val firstListParameter = parameterLists.head.parameters.head

    if (firstListParameter.isImplicit || firstListParameter.tpe.declarationId == UNIT_DECLARATIONID)
      return false

    if (parameterLists.size > 2)
      return false

    if (parameterLists.size == 2 && !parameterLists(1).parameters.forall(param => param.isImplicit))
      return false
    true
  }

  def getConversionTypes(returnType: TypeRef): (TypeRef, TypeRef) = {
    val fromType = returnType.typeArguments.head
    val toType = returnType.typeArguments.tail.head
    (fromType, toType)
  }

  def getConversionTypes(parameterLists: Seq[ParameterList], returnType: TypeRef): (TypeRef, TypeRef) = {
    val firstParameterList = parameterLists.head.parameters
    (firstParameterList.head.tpe, returnType)
  }

  // TODO some test could be created for this weird method
  // adjusts relative path by removing "../"
  // paths containing "../home/" and starting with "../" are prepended with "../"
  // relative path needs to be adjusted to match paths in module
  private def adjustPath(relativePath: String): String = {
    if (relativePath.startsWith("../"))
      if (relativePath.contains("../home/"))
        if (relativePath.contains("/sdk/"))
          "../" + relativePath
        else
          relativePath
      else
        relativePath.lastIndexOf("../") match {
          case -1 => relativePath
          case index => relativePath.substring(index + 2)
        }
    else
      relativePath
  }

  private def debug(path: String): Unit = {
    println("\"" + path + "\"")
    println(s"path size ${path.length}")
    println("char at 0 pos:" + path.charAt(0).toLong)
    println("Position of first \"/\" :" + path.indexOf("/"))
    println(s"Has non printable chars: ${hasNonprintableAsciiChar(path)}")
    println()
  }

  private def debugPaths(path: String, paths: Iterable[String]): Unit = {
    val contains = path.split("/").last
    println("paths containing \"" + contains + "\":")
    paths.foreach(path => if (path.contains(contains)) println(path))
    println()
  }

  private def hasNonprintableAsciiChar(s: String): Boolean = {
    val pattern = """[^\x20-\x7E]+""".r
    pattern.findFirstMatchIn(s) match {
      case Some(_) => true
      case None => false
    }
  }
}
