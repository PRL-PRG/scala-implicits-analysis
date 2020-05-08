package cz.cvut.fit.prl.scala.implicits.tools.neo4j

import cz.cvut.fit.prl.scala.implicits.model.{ClasspathEntry, Declaration, Module, PathEntry, SourcepathEntry, TypeRef}

object Utils {

  def getGroupArtifact(declaration: Declaration)(implicit module: Module): (String, String) = {
    // TODO Does the path needs to be somehow adjusted?
    // relative path needs to be adjusted to match paths in module
    val path = adjustPath(declaration.location.path)

    val entryPathOpt = module.paths.get(path)

    if (entryPathOpt.isEmpty) {
      println("Adjusted path")
      debug(path)
      println("Raw path")
      debug(declaration.location.path)
      debugPaths("android.jar", module.paths.keys)
    }

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
    relativePath.lastIndexOf("../") match {
      case -1 => relativePath
      case index =>
        val adjusted = relativePath.substring(index + 2)
        if (adjusted.startsWith("/home"))
          adjusted.split("/", 4)(3)
        else adjusted
    }
  }

  private def debug(path: String): Unit = {
    println("\"" + path + "\"")
    println(s"path size ${path.length}")
    println("char at 0 pos:" + path.charAt(0).toLong)
    println("Position of first \"/\" :" + path.indexOf("/"))
    println(s"Has non printable chars: ${hasNonprintableAsciiChar(path)}")
    println()
  }

  private def debugPaths(contains: String, paths: Iterable[String]): Unit = {
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
