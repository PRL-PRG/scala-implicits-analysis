package cz.cvut.fit.prl.scala.implicits.tools

import better.files._
import cz.cvut.fit.prl.scala.implicits.model.{Declaration, External, Local, Position, Project, SourcePath}
import cz.cvut.fit.prl.scala.implicits.utils._
import org.neo4j.graphdb.factory.GraphDatabaseFactory
import org.neo4j.graphdb.{GraphDatabaseService, Node, Relationship, RelationshipType}

import scala.collection.JavaConverters._
import scala.collection.concurrent.TrieMap
import scala.util.Try

case class Library(groupId: String, artifactId: String, version: String)

// TODO: support for scala versions
// TODO: support for projects references
// TODO: support for JDK
// TODO: support for maven
object Library {
  // library paths in Ivy are like
  // .../.ivy/cache/org.scala-lang/scala-library/jars/scala-library-2.12.6.jar
  private val LibraryRegex = "/([^/]+)/(jars|bundles)/([^/]+)-([^/]+)\\.jar$".r

  def apply(path: String): Option[Library] =
    LibraryRegex.findFirstMatchIn(path).map(x => Library(x.group(1), x.group(3), x.group(4)))

  def unapply(path: String): Option[(String, String, String, String)] =
    apply(path)
      .map(x => (x.groupId, x.artifactId, x.version, path))
}

object Converter {

  implicit class ProjectOps(project: Project) {
    def githubUrl: String = project.projectId.split("--").mkString("http://github.com/", "/", "")
  }

  implicit class PathOps(path: String) {
    def projectRelative(projectId: String): String =
      path.substring(path.indexOf(projectId) + projectId.length + 1)

    def library: Option[Library] = Library(path)
  }

  implicit class NodeOps(node: Node) {
    def children(relationshipType: RelationshipType, p: Relationship => Boolean): List[Relationship] = {
      node.getRelationships(relationshipType).iterator().asScala.filter(p).toList
    }
  }

}

class Converter(db: GraphDatabaseService) {

  import Converter._

  // TODO: this is not necessary - perhaps better is to have them separated by project
  // to avoid supernode (or densenode) situation -- need to be tested.
  val libraryGroupNodes = TrieMap.empty[String, Node]
  // (libraryGroupNode -> Version) -> libraryNode
  val libraryNodes = TrieMap.empty[(Node, String), Node]
  // (libraryNode -> Version) -> classpathNode
  val classpathNodes = TrieMap.empty[(Node, String), Node]

  def createSourcePath(sourcePath: SourcePath): Node = {
    val node = db.createNode(Labels.SourcePath, Labels.Path)

    node.setProperty("path", sourcePath.path)
    node.setProperty("kind", sourcePath.kind)
    node.setProperty("scope", sourcePath.scope)

    node
  }

  def createDeclaration(declaration: Declaration): Node = {
    val node = db.createNode(Labels.Declaration)

    node.setProperty("fqn", declaration.fqn)
    node.setProperty("name", declaration.name)
    node.setProperty("implicit", declaration.isImplicit)
    node.setProperty("kind", declaration.kind.value)

    node
  }

  def createLibraryGroup(groupId: String): Node = {
    val node = db.createNode(Labels.LibraryGroup)
    node.setProperty("groupId", groupId)
    node
  }

  def createLibrary(libraryGroupNode: Node, artifactId: String): Node = {
    val node = db.createNode(Labels.Library)
    node.setProperty("artifactId", artifactId)
    // TODO: should be the other way around
    libraryGroupNode.createRelationshipTo(node, Relationships.HAS_ARTIFACT)
    node
  }

  def createClasspath(libraryNode: Node, version: String): Node = {
    val node = db.createNode(Labels.Classpath)
    node.setProperty("version", version)
    // TODO: should be the other way around
    libraryNode.createRelationshipTo(node, Relationships.HAS_VERSION)
    node
  }

  def createProject(project: Project): Node = {
    val projectId = project.projectId
    val projectNode = db.createNode(Labels.Project)
    projectNode.setProperty("projectId", projectId)

    val classPaths =
      project
        .classpaths
        .map(x => Try(x -> x.library.getOrThrow(new IllegalArgumentException(s"$x is not an Ivy library"))))
        .split()

    val classPathsMap = classPaths._1.map { case (path, library) =>
      val groupNode = libraryGroupNodes.getOrElseUpdate(library.groupId, createLibraryGroup(library.groupId))

      val libraryNode = libraryNodes.getOrElseUpdate(
        (groupNode, library.artifactId),
        createLibrary(groupNode, library.artifactId)
      )

      val classpathNode = classpathNodes.getOrElseUpdate(
        (libraryNode, library.version),
        createClasspath(libraryNode, library.version)
      )

      path -> classpathNode
    }

    // TODO: collect all errors
    classPaths._2.foreach(x => println(x.getMessage))

    // normalize path to the project origin
    val sourcePathsMap =
      project
        .sourcepaths
        .map(x => x.copy(path = x.path.projectRelative(projectId)))
        .map(x => x.path -> createSourcePath(x))

    project.declarations.foreach { declaration =>
      val declarationNode = createDeclaration(declaration)
      projectNode.createRelationshipTo(declarationNode, Relationships.HAS_DECLARATION)

      val locationSetter: Option[Node => Unit] = declaration.location match {
        case Local(path, position) =>
          sourcePathsMap.collectFirst { case (dir, n) if path.startsWith(dir) => localSourceLocation(
            n,
            position,
            s"${project.githubUrl}/tree/master/$path#L${position.startLine}"
          )
          }
        case External(_, path, entry) =>
          classPathsMap.collectFirst { case (jar, n) if path == jar => externalSourceLocation(n, entry) }
        case _ =>
          None
      }

      locationSetter
        .getOrElse((_: Node) => println(s"No location relationship for ${declaration.location}"))
        .apply(declarationNode)
    }

    projectNode
  }

  def externalSourceLocation(classpathNode: Node, entry: String): Node => Unit = { declarationNode =>
    val rel = declarationNode.createRelationshipTo(classpathNode, Relationships.LOCATED_IN)

    rel.setProperty("entry", entry)
  }

  def localSourceLocation(sourcePathNode: Node, position: Position, url: String): Node => Unit = { declarationNode =>
    val rel = declarationNode.createRelationshipTo(sourcePathNode, Relationships.LOCATED_IN)

    rel.setProperty("startLine", position.startLine)
    rel.setProperty("endLine", position.endLine)
    rel.setProperty("startCol", position.startCol)
    rel.setProperty("endCol", position.endCol)
    rel.setProperty("url", url)
  }
}

object ImplicitsToNeo4j extends App {

  def createDatabase(path: File): GraphDatabaseService = {
    val dbFactory = new GraphDatabaseFactory
    dbFactory.newEmbeddedDatabase(path.toJava)
  }

  def deleteAll(implicit db: GraphDatabaseService): Unit = {
    val transaction = db.beginTx()

    //db.execute("MATCH (n) DETACH DELETE n")

    try {
      transaction.success()
      db.getAllRelationships.forEach(_.delete())
      db.getAllNodes.forEach(_.delete())
    } catch {
      case e: Throwable =>
        e.printStackTrace()
        transaction.failure()
    } finally {
      transaction.close()
    }
  }

  def run(): Unit = {
    val DbPath = "/home/krikava/Research/Projects/scala-corpus/corpora/4-top500/neo4j/databases/graph.db"
    val ImplicitsPath = "/home/krikava/Research/Projects/scala-corpus/corpora/4-top500/implicits.bin"
    implicit val db: GraphDatabaseService = createDatabase(File(DbPath))

    deleteAll

    val transaction = db.beginTx()
    val converter = new Converter(db)
    try {
      File(ImplicitsPath).inputStream.apply { input =>
        Project.streamFromDelimitedInput(input).foreach { project =>
          val time = System.currentTimeMillis()
          converter.createProject(project)
          println(s"Converted ${project.projectId} in ${System.currentTimeMillis() - time}ms")
        }
      }

      transaction.success()
    } catch {
      case e: Throwable =>
        e.printStackTrace()
        transaction.failure()
    } finally {
      transaction.close()
      db.shutdown()
    }
  }

  run()
}
