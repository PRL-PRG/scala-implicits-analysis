package cz.cvut.fit.prl.scala.implicits.tools.neo4j

import cz.cvut.fit.prl.scala.implicits.model.{Declaration, Module, TypeRef}
import cz.cvut.fit.prl.scala.implicits.tools.graphDbEntities.{Labels, Relationships}
import org.neo4j.graphdb.{Direction, Node, Transaction}

import scala.collection.JavaConverters._


class Proxy(transaction: Transaction, cache: NodesCache) {

  def mergeTypeReferenceNode(tpe: TypeRef)(implicit module: Module, moduleNode: Node): Node = {
    val declaration = module.declarations(tpe.declarationId)
    val (groupId, artifactId) = Utils.getGroupArtifact(declaration)(module)

    val declarationNode = mergeDeclarationNode(declaration, artifactId, groupId)(module, moduleNode)

    val typeExpression = Utils.getTypeExpression(tpe)

    val typeRefNodeOpt = declarationNode.getRelationships(Direction.INCOMING, Relationships.TYPEREF_DECLARATION)
      .iterator
      .asScala
      .find(_.getStartNode.getProperty("typeExpression") == typeExpression)
      .map(_.getStartNode)

    if (typeRefNodeOpt.nonEmpty) typeRefNodeOpt.get
    else {
      val typeRefNode = createNode(Labels.TypeReference, Map("typeExpression" -> typeExpression))
      typeRefNode.createRelationshipTo(declarationNode, Relationships.TYPEREF_DECLARATION)
      typeRefNode
    }
  }


  // Gets declaration node, if it is available or creates new one with the connection to the artifactId and groupId
  // Could be simplified by creating all groupIds and artifacts beforehand
  // TODO Edit - at least create sub-functions
  // TODO refactor java stream to scala iterator - conversion cost?
  // TODO Module declares Declaration relationShip wont be ever created in case module does not declare this declaration
  //  - Is this relation necessary? This relation-ship is already present in form of artifact relation
  def mergeDeclarationNode(declaration: Declaration, artifactId: String, groupId: String)(implicit module:Module, moduleNode: Node): Node = {
    val groupNodeOpt = transaction.findNodes(Labels.Group, "groupId", groupId).asScala.find(_ => true)
    // group, artifact, declaration needs to be created
    if (groupNodeOpt.isEmpty) {
      val groupNode = createNode(Labels.Group, Map("groupId" -> groupId))
      val artifactNode = createNode(Labels.Artifact, Map("artifactId" -> artifactId))
      val declarationNode = createDeclarationNode(declaration)

      groupNode.createRelationshipTo(artifactNode, Relationships.GROUP_ARTIFACT)
      artifactNode.createRelationshipTo(declarationNode, Relationships.ARTIFACT_DECLARATION)
      if (module.artifactId == artifactId && module.groupId == groupId)
        moduleNode.createRelationshipTo(declarationNode, Relationships.DECLARES)
      declarationNode
    }
    else {
      val groupNode = groupNodeOpt.get

      val artifactNodeOpt = groupNode.getRelationships(Direction.OUTGOING, Relationships.GROUP_ARTIFACT).asScala
        .find(relationShip => relationShip.getEndNode.getProperty("artifactId").equals(artifactId))
        .map(_.getEndNode)

      // artifact, declaration needs to be created
      if (artifactNodeOpt.isEmpty) {
        //
        val artifactNode = createNode(Labels.Artifact, Map("artifactId" -> artifactId))
        val declarationNode = createDeclarationNode(declaration)

        groupNode.createRelationshipTo(artifactNode, Relationships.GROUP_ARTIFACT)
        artifactNode.createRelationshipTo(declarationNode, Relationships.ARTIFACT_DECLARATION)
        if (module.artifactId == artifactId && module.groupId == groupId)
          moduleNode.createRelationshipTo(declarationNode, Relationships.DECLARES)
        declarationNode
      }
      else {
        val artifactNode = artifactNodeOpt.get

        artifactNode.getRelationships(Direction.OUTGOING, Relationships.ARTIFACT_DECLARATION).asScala.
          find(relation => relation.getEndNode.getProperty("declarationId") == declaration.declarationId)
          .map(_.getEndNode)
          .getOrElse({
            val declarationNode = createDeclarationNode(declaration)

            artifactNode.createRelationshipTo(declarationNode, Relationships.ARTIFACT_DECLARATION)
            if (module.artifactId == artifactId && module.groupId == groupId)
              moduleNode.createRelationshipTo(declarationNode, Relationships.DECLARES)
            declarationNode
          })
      }
    }
  }

  private def createDeclarationNode(declaration: Declaration): Node = {
    val properties = Map("declarationId" -> declaration.declarationId,
      "name" -> declaration.name,
      "properties" -> declaration.properties)
    val declarationNode = createNode(Labels.Declaration, properties)


    val accessNode = mergeNode(Labels.Access, Map("name" -> declaration.access.toString()))
    declarationNode.createRelationshipTo(accessNode, Relationships.HAS_ACCESS)

    val languageNode = mergeNode(Labels.Language, Map("name" -> declaration.language.toString()))
    declarationNode.createRelationshipTo(languageNode, Relationships.IN_LANGUAGE)

    val declarationTypeNode = mergeNode(Labels.DeclarationType, Map("name" -> declaration.kind.toString()))
    declarationNode.createRelationshipTo(declarationTypeNode, Relationships.DECLARATION_TYPE)

    declarationNode
  }

  private def mergeNode(label: Labels, properties: Map[String, Object]): Node =
    transaction.findNodes(label, properties.asJava).stream()
      .findFirst()
      .orElseGet(() => createNode(label, properties))

  private def mergeNode(label:Labels): Node =
    transaction.findNodes(label).stream()
      .findFirst()
      .orElseGet(() => createNode(label))

  private def createNode(label: Labels, properties: Map[String, Any]): Node =
    setNodeProperties(transaction.createNode(label), properties)

  private def createNode(label: Labels): Node =
    transaction.createNode(label)

  private def setNodeProperties(node: Node, properties: Map[String, Any]): Node = {
    properties.foreach(property => node.setProperty(property._1, property._2))
    node
  }

}

object Proxy{
  def apply(transaction: Transaction) = new Proxy(transaction, NodesCache())
}