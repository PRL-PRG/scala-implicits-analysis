package cz.cvut.fit.prl.scala.implicits.tools

import better.files.File
import cz.cvut.fit.prl.scala.implicits.model.Util._
import cz.cvut.fit.prl.scala.implicits.model.{CallSite, ClasspathEntry, Declaration, Location, Module, PathEntry, Project, SourcepathEntry}


object ImplicitsPathAdjuster extends App {


  def apply(implicitsFile: File, outputImplicitsFile: File): Unit = {
    val outputStream = outputImplicitsFile.outputStream

    outputStream.apply(os => {
      implicitsFile.inputStream.apply(
        input => Project.streamFrom(input).foreach(
          project => {
            Project.write(fixProjectsPaths(project), os)
          }
        )
      )
    })
  }

  def fixProjectsPaths(project: Project): Project = {
    val newModules = project.modules.map {
      case (moduleId, module) => (moduleId, modifyModule(module))
    }

    Project(project.projectId, project.sbtVersion, newModules)
  }


  private def modifyModule(module: Module): Module = {
    val newPaths = module.paths.map {
      case (path, pathEntry) =>
        (modifyPath(path), modifyEntryPath(pathEntry))
    }
    val newDeclarations = module.declarations.map {
      case (declarationId, declaration) => (declarationId, modifyDeclaration(declaration))
    }
    val newCallSites = module.implicitCallSites.map(callSite => modifyCallSite(callSite))

    Module(
      module.moduleId, module.projectId, module.groupId, module.artifactId, module.version, module.commit,
      module.scalaVersion, newPaths, newDeclarations, newCallSites,
      module.callSitesCount, module.testCallSitesCount
    )
  }

  private def modifyDeclaration(declaration: Declaration):Declaration = {
    val newLocation = modifyLocation(declaration.location)
    Declaration(declaration.declarationId, declaration.moduleId, declaration.kind, declaration.properties,
      declaration.name, declaration.access, newLocation, declaration.language,
      declaration.annotations, declaration.signature)
  }

  private def modifyCallSite(callSite: CallSite): CallSite = {
    val newLocation = modifyLocation(callSite.location)
    CallSite(callSite.callSiteId, callSite.parentId, callSite.moduleId, callSite.declarationId,
      callSite.code, newLocation, callSite.typeArguments, callSite.implicitArgumentTypes)
  }

  private def modifyEntryPath(pathEntry: PathEntry): PathEntry = {
    pathEntry match {
      case ClasspathEntry(path, groupId, artifactId, version, scope, internal, managed, transitive) =>
        ClasspathEntry(modifyPath(path), groupId, artifactId, version, scope, internal, managed, transitive)
      case SourcepathEntry(path, scope, managed) =>
        SourcepathEntry(modifyPath(path), scope, managed)
      case _ => throw new IllegalArgumentException("This wont happen")
    }
  }


  private def modifyLocation(location: Location): Location = {
    Location(modifyPath(location.path), location.relativeUri, location.position)
  }

  private def modifyPath(path: String): String = {
    val path1 = path.lastIndexOf("../") match {
      case -1 => path
      case index => path.substring(index + 2)
    }

    if (path1.startsWith("/"))
      path1.substring(1)
    else
      path1
  }

  val corporaDir = "/home/panpuncocha/skola/bt/OOPSLA19-artifact/corpora/"

  val projectDir = corporaDir + "2-single"
  val implicitsBinRelPath = "/implicits.bin"
  val newFileName = "/implicits2.bin"

  apply(File(projectDir + implicitsBinRelPath), File(projectDir + newFileName))
}
