package cz.cvut.fit.prl.scala.implicits.tools.neo4j

import java.util

import cz.cvut.fit.prl.scala.implicits.tools.graphDbEntities.Labels
import org.mockito.Mockito
import org.neo4j.graphdb.{Label, Node, ResourceIterator, Transaction}
import org.scalatest.mockito.MockitoSugar
import org.scalatest.{FunSuite, Matchers}

import scala.collection.JavaConverters._

class ProxySuite extends FunSuite with Matchers with MockitoSugar {

  implicit class ResourceIteratorImpl[T](list: util.ArrayList[T]) extends ResourceIterator[T] {
    override def close(): Unit = {}

    override def hasNext: Boolean = !list.isEmpty

    override def next(): T = {
      val n = list.get(0)
      list.remove(0)
      n
    }
  }

  test("createNode") {
    implicit val transaction: Transaction = mock[Transaction]
    val declarationNode = mock[Node]


    val listOfLabels = new util.ArrayList[Label](util.Arrays.asList(Labels.Declaration))
    Mockito.when(declarationNode.getLabels) thenReturn listOfLabels

    Mockito.when(transaction.createNode(Labels.Declaration)) thenReturn declarationNode
    val proxy = Proxy()
    val createdNode = proxy.createNode(Labels.Declaration)

    createdNode.getLabels.asScala.head shouldEqual Labels.Declaration
  }

  test("createNodeSetProperties") {
    implicit val transaction: Transaction = mock[Transaction]
    val declarationNode = mock[Node]
    val propertyName = "name"
    val propertyValue = "value"

    Mockito.when(transaction.createNode(Labels.Declaration)) thenReturn declarationNode

    val proxy = Proxy()
    val createdNode = proxy.createNode(Labels.Declaration, Map(propertyName -> propertyValue))
    Mockito.verify(declarationNode).setProperty(propertyName, propertyValue)

    createdNode shouldEqual declarationNode
  }

  test("mergeNode1") {
    implicit val transaction: Transaction = mock[Transaction]
    val declarationNode = mock[Node]

    val listOfLabels: ResourceIteratorImpl[Node] = ResourceIteratorImpl(new util.ArrayList[Node](util.Arrays.asList(declarationNode)))


    Mockito.when(transaction.findNodes(Labels.Declaration)) thenReturn listOfLabels

    val proxy = Proxy()
    val mergedNode = proxy.mergeNode(Labels.Declaration)

    mergedNode shouldEqual declarationNode
  }

  test("mergeNode2") {
    implicit val transaction: Transaction = mock[Transaction]
    val declarationNode = mock[Node]

    val listOfLabels: ResourceIteratorImpl[Node] = ResourceIteratorImpl(new util.ArrayList[Node](util.Arrays.asList()))

    Mockito.when(transaction.findNodes(Labels.Declaration)) thenReturn listOfLabels
    Mockito.when(transaction.createNode(Labels.Declaration)) thenReturn declarationNode


    val proxy = Proxy()
    val mergedNode = proxy.mergeNode(Labels.Declaration)

    mergedNode shouldEqual declarationNode
  }


//  def createNodesCache(): NodesCache = {
//    val cache = NodesCache()
//    // todo add something to the cache
//    cache
//  }

//  test("mergeDeclarationNode") {
//    val moduleGroupId = "mGroupId"
//    val moduleArtifactId = "mArtifactId"
//
//    implicit val transaction: Transaction = mock[Transaction]
//    implicit val module: Module = Module(null, null, moduleGroupId, moduleArtifactId, null, null, null, null, null, null, 1, 1)
//    implicit val moduleNode: Node = mock[Node]
//
//
//    val groupNode = mock[Node]
//    val artifactNode = mock[Node]
//    val declarationNode = mock[Node]
//    val accessNode = mock[Node]
//    val languageNode = mock[Node]
//    val declarationTypeNode = mock[Node]
//
//    Mockito.when(groupNode.getId) thenReturn 1
////    Mockito.verify(groupNode).createRelationshipTo(artifactNode, Relationships.GROUP_ARTIFACT)
//
//    Mockito.when(artifactNode.getId) thenReturn 2
////    Mockito.verify(artifactNode).createRelationshipTo(declarationNode, Relationships.ARTIFACT_DECLARATION)
//
//    Mockito.when(declarationNode.getId) thenReturn 3
////    Mockito.verify(moduleNode).createRelationshipTo(declarationNode, Relationships.DECLARES)
//
//    Mockito.when(transaction.createNode(Labels.Group)) thenReturn groupNode
//    Mockito.when(transaction.createNode(Labels.Artifact)) thenReturn artifactNode
//    Mockito.when(transaction.createNode(Labels.Declaration)) thenReturn declarationNode
//
////    Mockito.when(transaction.findNodes(Labels.Access, Map[String,Object]("Access" -> "PRIVATE").asJava)) thenReturn  accessNode
////    Mockito.when(transaction.findNodes(Labels.Language, Map[String,Object]("Language" -> "SCALA").asJava)) thenReturn languageNode
//    //
////
////    val accessNode = mergeNode(Labels.Access, Map("name" -> declaration.access.toString()))
////    declarationNode.createRelationshipTo(accessNode, Relationships.HAS_ACCESS)
////
////    val languageNode = mergeNode(Labels.Language, Map("name" -> declaration.language.toString()))
////    declarationNode.createRelationshipTo(languageNode, Relationships.IN_LANGUAGE)
////
////    val declarationTypeNode = mergeNode(Labels.DeclarationType, Map("name" -> declaration.kind.toString()))
////    declarationNode.createRelationshipTo(declarationTypeNode, Relationships.DECLARATION_TYPE)
//
//
//
//
//    val propertiesVal = 0
//    val declarationGroupId = moduleGroupId
//    val declarationArtifactId = moduleArtifactId
//    val declaration = Declaration("declId", null, Declaration.Kind.CLASS, propertiesVal, "declName", Declaration.Access.PRIVATE, null, Language.SCALA, Seq(), null)
//
//
//    val proxy = new Proxy(createNodesCache())
//    proxy.mergeDeclarationNode(declaration, declarationArtifactId, declarationGroupId) shouldEqual declarationNode
//  }
}
