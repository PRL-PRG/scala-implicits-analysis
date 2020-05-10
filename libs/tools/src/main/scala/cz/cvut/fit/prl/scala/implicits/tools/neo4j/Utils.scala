package cz.cvut.fit.prl.scala.implicits.tools.neo4j

import cz.cvut.fit.prl.scala.implicits.model.Declaration.Kind.TYPE
import cz.cvut.fit.prl.scala.implicits.model.Language.UNKNOWN_LANGUAGE
import cz.cvut.fit.prl.scala.implicits.model.{ClasspathEntry, Declaration, Location, Module, PathEntry, SourcepathEntry, TypeRef, TypeSignature}

object Utils {

  val UNKNOWN_GROUP_ID = "UNKNOWN_GROUP_ID"
  val UNKNOWN_ARTIFACT_ID = "UNKNOWN_ARTIFACT_ID"

  def getGroupArtifact(declaration: Declaration)(implicit module: Module): (String, String) = {
    // TODO Does the path needs to be somehow adjusted?
    // relative path needs to be adjusted to match paths in module
//    val path = adjustPath(declaration.location.path)
    val path = declaration.location.path

    // TODO remove only for debugging purposes
    val entryPathOpt = module.paths.get(path)
    if (entryPathOpt.isEmpty) {
      println("Adjusted path")
      debug(path)
      println("Raw path")
      debug(declaration.location.path)
      debugPaths(path, module.paths.keys)
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


  def createUnknownDeclaration(): Declaration = {
    Declaration("UNKNOW","", TYPE,0,"",Declaration.Access.NOT_SPECIFIED,Location("","",None),UNKNOWN_LANGUAGE, Seq.empty, TypeSignature())
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
