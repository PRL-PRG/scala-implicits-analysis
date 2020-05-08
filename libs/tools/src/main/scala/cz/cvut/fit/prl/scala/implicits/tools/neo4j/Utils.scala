package cz.cvut.fit.prl.scala.implicits.tools.neo4j

import cz.cvut.fit.prl.scala.implicits.model.{ClasspathEntry, Declaration, Module, PathEntry, SourcepathEntry, TypeRef}

object Utils {

  def getGroupArtifact(declaration: Declaration)(implicit module: Module): (String, String) = {
    // TODO Does the path needs to be somehow adjusted?
    // relative path needs to be adjusted to match paths in module
    val path = adjustPath(declaration.location.path)

    val entryPath = module.paths(path)

    // TODO - create some path nodes? Or should they be ignored?
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

  // relative path needs to be adjusted to match paths in module
  private def adjustPath(relativePath: String): String = {
    if (relativePath.startsWith("/home")) {
      println("I was here in adjust path starts with /home")
      println(s"relalative path: $relativePath\n adjusted: ${relativePath.split("/", 4)(3)}")
      return relativePath.split("/", 4)(3)
    }
    relativePath.lastIndexOf("../") match {
      case -1 => relativePath
      case index => relativePath.substring(index + 2)
    }
  }
}
