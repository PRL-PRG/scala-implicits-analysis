package cz.cvut.fit.prl.scala.implicits.tools

import java.io.{FileInputStream, FileOutputStream}
import java.util.Optional

import better.files.File
import cz.cvut.fit.prl.scala.implicits.model.{CallSite, ClasspathEntry, Declaration, Module, PathEntry, Position, Project, SourcepathEntry}
import cz.cvut.fit.prl.scala.implicits.utils._
import cz.cvut.fit.prl.scala.implicits.model.Util._
import org.neo4j.dbms.api.{DatabaseManagementService, DatabaseManagementServiceBuilder}
import org.neo4j.graphdb.{GraphDatabaseService, Node, Relationship, RelationshipType, ResourceIterable, ResourceIterator, Transaction}

import scala.collection.JavaConverters._
import scala.collection.concurrent.TrieMap
import scala.util.Try

//case class Library(groupId: String, artifactId: String, version: String)
//
//// TODO: support for scala versions
//// TODO: support for projects references
//// TODO: support for JDK
//// TODO: support for maven
//object Library {
//  // library paths in Ivy are like
//  // .../.ivy/cache/org.scala-lang/scala-library/jars/scala-library-2.12.6.jar
//  private val LibraryRegex = "/([^/]+)/(jars|bundles)/([^/]+)-([^/]+)\\.jar$".r
//
//  def apply(path: String): Option[Library] =
//    LibraryRegex
//      .findFirstMatchIn(path)
//      .map(x => Library(x.group(1), x.group(3), x.group(4)))
//
//  def unapply(path: String): Option[(String, String, String, String)] =
//    apply(path)
//      .map(x => (x.groupId, x.artifactId, x.version, path))
//}

object Converter {

  implicit class ProjectOps(project: Project) {

    def githubUrl: String =
      project.projectId.split("--").mkString("http://github.com/", "/", "")
  }

  implicit class PathOps(path: String) {

    def projectRelative(projectId: String): String =
      path.substring(path.indexOf(projectId) + projectId.length + 1)

//    def library: Option[Library] = Library(path)
  }

  implicit class NodeOps(node: Node) {

    def children(
        relationshipType: RelationshipType,
        p: Relationship => Boolean): List[Relationship] = {
      node.getRelationships(relationshipType).iterator().asScala.filter(p).toList
    }
  }

}

class Converter(transaction: Transaction) {

  import Converter._

//  // TODO: this is not necessary - perhaps better is to have them separated by project
//  // to avoid supernode (or densenode) situation -- need to be tested.
//  val libraryGroupNodes = TrieMap.empty[String, Node]
//  // (libraryGroupNode -> Version) -> libraryNode
//  val libraryNodes = TrieMap.empty[(Node, String), Node]
//  // (libraryNode -> Version) -> classpathNode
//  val classpathNodes = TrieMap.empty[(Node, String), Node]


//  def createLibrary(libraryGroupNode: Node, artifactId: String): Node = {
//    val node = transaction.createNode(Labels.Library)
//    node.setProperty("artifactId", artifactId)
//    // TODO: should be the other way around
//    libraryGroupNode.createRelationshipTo(node, Relationships.HAS_ARTIFACT)
//    node
//  }

  def createDeclarationNode(declaration: Declaration): Node = {
    // TODO create conections for language, kind, access
    val properties = Map("declarationId" -> declaration.declarationId,
      "name" -> declaration.name,
      "properties" -> declaration.properties)
    createNode(Labels.Declaration, properties)
  }
  // Gets declaration node, if it is available or creates new one with the connection to the artifactId and groupId
  def mergeDeclarationNode(declaration: Declaration, artifactId: String, groupId: String): Node = {

    val declarationNodeOpt = transaction.findNodes(Labels.Declaration, "declarationId", declaration.declarationId).stream()
      .findFirst()

    if (declarationNodeOpt.isPresent)
      return declarationNodeOpt.get()

    val declarationNode = createDeclarationNode(declaration)

    val artifactNodeOpt = transaction.findNodes(Labels.Artifact, "artifactId", artifactId).stream()
      .findFirst()

    if (artifactNodeOpt.isPresent) {
      artifactNodeOpt.get().createRelationshipTo(declarationNode, Relationships.ARTIFACT_DECLARATION)
      return declarationNode
    }

    val artifactNode = createNode(Labels.Artifact, Map("artifactId" -> artifactId))
    artifactNode.createRelationshipTo(declarationNode, Relationships.ARTIFACT_DECLARATION)

    val groupNode = mergeNode(Labels.Group, Map("groupId" -> groupId))

    groupNode.createRelationshipTo(artifactNode, Relationships.GROUP_ARTIFACT)

    declarationNode
  }


  // more types of call sites, solve it buy groupId?
  def createCallSiteNode(callSite: CallSite): Node = {
  // TODO resolve parent callSite (callSite hiearchy)?
    // What is the callSiteId?
    val properties = Map(("callSiteId", callSite.callSiteId), ("code", callSite.code))
    val callSiteNode = createNode(Labels.CallSite, properties)
    callSiteNode
  }

//  // What label and relation ship name should be used for childs of PathEntry? How should this inheritance should be dealt with?
//  def createPathNode(pathEntry: PathEntry): Node = {
//    val pathProperties: Map[String, Object] = pathEntry match {
////        Converting scala types to java object
////      case SourcepathEntry(path, scope, managed) => Map("path" -> path, "scope" -> scope, "managed" -> managed)
////      case ClasspathEntry(path, groupId, artifactId, scope, internal, managed, version, transitive) =>
////        Map("path" -> path, "scope" -> scope, "managed" -> managed, "groupId" -> groupId,
////          "artifactId" -> artifactId, "internal" -> internal, "version" -> version, "transitive" -> transitive)
//      case SourcepathEntry(path, scope, managed) => Map("path" -> path, "scope" -> scope)
//      case ClasspathEntry(path, groupId, artifactId, scope, internal, managed, version, transitive) =>
//        Map("path" -> path, "scope" -> scope, "groupId" -> groupId,
//          "artifactId" -> artifactId, "internal" -> internal)
//      case _ => Map()
//    }
//    mergeNode(Labels.Path, pathProperties)
//  }

  def createModuleNode(module: Module): Node = {
    val moduleProperties = Map(("moduleId", module.moduleId), ("groupId", module.groupId),
      ("scalaVersion", module.scalaVersion), ("artifactId", module.artifactId),("version", module.version),
      ("commit", module.commit))

    val moduleNode = createNode(Labels.Module, moduleProperties)

    module.implicitCallSites.foreach(callSite => {
      val callSiteNode = createCallSiteNode(callSite)
      moduleNode.createRelationshipTo(callSiteNode, Relationships.HAS_CALLSITE)

      val declaration : Declaration = module.declarations(callSite.declarationId)

      // TODO path needs to be adjusted somehow
      val path = declaration.location.path
      val entryPath = module.paths.get(declaration.location.path)

      if (entryPath.isEmpty) {
        println(s"Path $path does not match any paths")
      }
      else {
        // TODO - create some nodes from paths? Or should they be ignored?
        val declarationNode = entryPath.get match {
          case ClasspathEntry(_, groupId, artifactId, _, _, _, _, _) => {
            mergeDeclarationNode(declaration, artifactId, groupId)
          }
          case SourcepathEntry(_, _, _) => {
            val declNode = mergeDeclarationNode(declaration, module.artifactId, module.groupId)
            moduleNode.createRelationshipTo(declNode, Relationships.DECLARES)
            declNode
          }
          case _ => throw new IllegalArgumentException("No path found - is ensured, it wont come to here!")
        }
        callSiteNode.createRelationshipTo(declarationNode, Relationships.DECLARED_BY)
      }
    })

    moduleNode
  }

  def createProject(project: Project): Node = {
    val projectProperties = Map(("projectId", project.projectId),("sbtVersion", project.sbtVersion))
    val projectNode = mergeNode(Labels.Project, projectProperties)

    project.modules.foreach( moduleTuple  => {
      val (_, module) = moduleTuple
      val moduleNode = createModuleNode(module)
      projectNode.createRelationshipTo(moduleNode, Relationships.HAS_MODULE)
    })

    projectNode
  }


  private def mergeNode(label: Labels, properties: Map[String, Object]): Node =
    transaction.findNodes(label, properties.asJava).stream()
      .findFirst()
      .orElse(createNode(label, properties))

  private def createNode(label: Labels, properties: Map[String, Any]): Node =
    setNodeProperties(transaction.createNode(label), properties)

  private def setNodeProperties(node: Node, properties: Map[String, Any]): Node = {
    properties.foreach(property => node.setProperty(property._1, property._2))
    node
  }
}

object ImplicitsToNeo4j extends App {

  // Registers a shutdown hook for the Neo4j instance so that it
  // shuts down nicely when the VM exits (even if you "Ctrl-C" the
  // running application).
  private def registerShutdownHook(managementService: DatabaseManagementService): Unit = {
//    val t = new ShutdownHookThread(hookName()) {
//      override def run() = body
//    }

    sys.addShutdownHook(new Thread() {
      override def run(): Unit = {
        managementService.shutdown()
      }
    })
  }

  def cleanUpDatabase(implicit graphDb: GraphDatabaseService): Unit = {
    val transaction = graphDb.beginTx()

    try {
      transaction.execute("MATCH (n) DETACH DELETE n")
      transaction.commit()
    } catch {
      case e: Throwable =>
        e.printStackTrace()}
    finally {
      transaction.close()
    }
  }

  def run(): Unit = {
    val DEFAULT_DB_NAME = "neo4j"
    val corporaDir = "/home/panpuncocha/skola/bt/pipelineTest/OOPSLA19-artifact/corpora/"
//    val projectDir = corporaDir + "2-single"
//    val implicitsBinRelPath = "/implicits.bin"
    val projectDir = corporaDir + "1-example"
    val implicitsBinRelPath = "/_analysis_/implicits.bin"

    val implicitsPath = projectDir + implicitsBinRelPath

    val dbDirectoryRelPath = "/neo4j/databases"
    val dbDirectoryPath = File(projectDir + dbDirectoryRelPath).toJava

    // opening/creating new graph db
    val managementService = new DatabaseManagementServiceBuilder(dbDirectoryPath).build()
    implicit val graphDb: GraphDatabaseService = managementService.database(DEFAULT_DB_NAME)
    registerShutdownHook(managementService)

    cleanUpDatabase

    val transaction: Transaction = graphDb.beginTx()
    val converter = new Converter(transaction)
    try {

      File(implicitsPath).inputStream.apply(
        input => Project.streamFrom(input).foreach(
          project => {
            val time = System.currentTimeMillis()
            converter.createProject(project)
            println(
              s"Converted ${project.projectId} in ${System.currentTimeMillis() - time}ms")
          }
        )
      )


      transaction.commit()
    } catch {
      case e: Throwable =>
        e.printStackTrace()
    } finally {
      transaction.close()
      managementService.shutdown()
    }
  }

  run()
}
