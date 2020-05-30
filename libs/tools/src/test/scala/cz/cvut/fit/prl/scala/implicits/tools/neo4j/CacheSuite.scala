package cz.cvut.fit.prl.scala.implicits.tools.neo4j

import org.neo4j.graphdb.{Node, Transaction}
import org.scalamock.scalatest.MockFactory
import org.scalatest.{ FunSuite, Matchers}

class CacheSuite extends FunSuite with Matchers with MockFactory {

  private val dummyNode1 = mock[Node]
  private val dummyNode2 = mock[Node]
  private implicit val transaction: Transaction = mock[Transaction]

  test("insertOneGroup") {
    val nodeId = 1L
    (dummyNode1.getId _).expects().returning(nodeId).once()
    (transaction.getNodeById _).expects(nodeId).returning(dummyNode1).once()
    val cache = NodesCache()
    val groupId = "group1"

    cache.put(groupId, dummyNode1)
    cache.apply(groupId).get._1 shouldEqual dummyNode1
  }

  test("insertTwoGroups") {
    val nodeId1 = 1L
    val nodeId2 = 2L
    (dummyNode1.getId _).expects().returning(nodeId1).once()
    (dummyNode2.getId _).expects().returning(nodeId2).once()
    (transaction.getNodeById _).expects(nodeId1).returning(dummyNode1).once()

    val cache = NodesCache()
    val groupId1 = "group1"
    val groupId2 = "group2"


    cache.put(groupId1, dummyNode1)
    cache.put(groupId2, dummyNode2)


    cache.apply(groupId1).get._1 shouldEqual dummyNode1
  }

  test("getDeclarationTuplePositive") {
    val groupNodeId = 1L
    val artifactNodeId = 2L
    val declarationNodeId = 3L

    val groupNode = mock[Node]
    val artifactNode = mock[Node]
    val declarationNode = mock[Node]

    val groupId = "group"
    val artifactId = "artifact"
    val declarationId = "declarationId"

    (groupNode.getId _).expects().returning(groupNodeId).once()
    (artifactNode.getId _).expects().returning(artifactNodeId).once()
    (declarationNode.getId _).expects().returning(declarationNodeId).once()

    (transaction.getNodeById _).expects(declarationNodeId).returning(declarationNode).once()

    val cache = NodesCache()

    val artifactCache = cache.put(groupId, groupNode)
    val declarationCache = artifactCache.put(artifactId, artifactNode)
    declarationCache.put(declarationId, declarationNode)


    cache.getDeclarationTuple(groupId, artifactId, declarationId)._1 shouldEqual declarationNode
  }

  test("getDeclarationTupleNegative") {
    val groupNodeId = 1L
    val artifactNodeId = 2L
    val declarationNodeId = 3L

    val groupNode = mock[Node]
    val artifactNode = mock[Node]
    val declarationNode = mock[Node]

    val groupId = "group"
    val artifactId = "artifact"
    val declarationId = "declarationId"

    (groupNode.getId _).expects().returning(groupNodeId).once()
    (artifactNode.getId _).expects().returning(artifactNodeId).once()
    (declarationNode.getId _).expects().returning(declarationNodeId).once()


    val cache = NodesCache()

    val artifactCache = cache.put(groupId, groupNode)
    val declarationCache = artifactCache.put(artifactId, artifactNode)
    declarationCache.put(declarationId, declarationNode)

    an [NoSuchElementException] should be thrownBy cache.getDeclarationTuple(groupId, artifactId, declarationId + "some")
  }

}
