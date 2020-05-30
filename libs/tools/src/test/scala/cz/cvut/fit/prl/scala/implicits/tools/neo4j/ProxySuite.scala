package cz.cvut.fit.prl.scala.implicits.tools.neo4j

import java.{lang, util}

import org.neo4j.graphdb.{Entity, Label, Lock, Node, Relationship, RelationshipType, ResourceIterable, ResourceIterator, Result, StringSearchMode, Transaction}
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FunSuite, Matchers}
import cz.cvut.fit.prl.scala.implicits.model.{ClasspathEntry, Declaration, Language, Location, Module, Parameter, ParameterList, PathEntry, SourcepathEntry, TypeRef}
import cz.cvut.fit.prl.scala.implicits.tools.graphDbEntities.{Labels, Relationships}
import org.mockito.Mockito
import org.mockito.internal.matchers.Any
import org.neo4j.graphdb.schema.Schema
import org.neo4j.graphdb.traversal.{BidirectionalTraversalDescription, TraversalDescription}
import org.scalatest.mockito.MockitoSugar

import scala.collection.JavaConverters._
import scala.collection.immutable.HashMap

class ProxySuite extends FunSuite with Matchers with MockitoSugar {

  def createNodesCache(): NodesCache = {
    val cache = NodesCache()
    // todo add something to the cache
    cache
  }


//  test("mergeDeclarationNode") {
//    val transaction = mock[Transaction]
//    val node = mock[Node]
//
//    Mockito.when(transaction.createNode(Labels.Group)).thenReturn(node)
//
//    transaction.createNode(Labels.Group) shouldEqual node
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
