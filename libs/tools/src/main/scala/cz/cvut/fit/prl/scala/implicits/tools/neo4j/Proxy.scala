package cz.cvut.fit.prl.scala.implicits.tools.neo4j

import cz.cvut.fit.prl.scala.implicits.model.Declaration.Kind.{TYPE, VAL}
import cz.cvut.fit.prl.scala.implicits.model.Language.UNKNOWN_LANGUAGE
import cz.cvut.fit.prl.scala.implicits.model.{Declaration, Language, Location, Module, TypeRef, TypeSignature}
import cz.cvut.fit.prl.scala.implicits.tools.graphDbEntities.{Labels, Relationships}
import org.neo4j.graphdb.{Direction, Node, Transaction}

import scala.collection.JavaConverters._


class Proxy(transaction: Transaction, cache: NodesCache) {

  def mergeTypeReferenceNode(tpe: TypeRef)(implicit module: Module, moduleNode: Node): Node = {
    // This validation is only because there are some missing values in module.declarations in some projects
    val declarationOpt = module.declarations.get(tpe.declarationId)
    val (declaration, groupId, artifactId) = declarationOpt match {
      case Some(d) =>
        val (g, a) = Utils.getGroupArtifact(d)(module)
        (d, g, a)
      case None =>
        (Utils.createUnknownDeclaration(), Utils.UNKNOWN_GROUP_ID, Utils.UNKNOWN_ARTIFACT_ID)
    }

    val (declarationNode, typeReferenceCache) = cache.getDeclarationTuple(groupId, artifactId, declaration)

    val typeExpression = Utils.getTypeExpression(tpe)

    typeReferenceCache(typeExpression).getOrElse({
      val typeRefNode = createNode(Labels.TypeReference, Map("typeExpression" -> typeExpression))
      typeReferenceCache.put(typeExpression, typeRefNode)
      typeRefNode.createRelationshipTo(declarationNode, Relationships.TYPEREF_DECLARATION)
      typeRefNode
    })
  }


  // Gets declaration node, if it is available or creates new one with the connection to the artifactId and groupId
  // Could be simplified by creating all groupIds and artifacts beforehand
  // TODO Edit - at least create sub-functions
  // TODO refactor java stream to scala iterator - conversion cost?
  // TODO Module declares Declaration relationShip wont be ever created in case module does not declare this declaration
  //  - Is this relation necessary? This relation-ship is already present in form of artifact relation
  def mergeDeclarationNode(declaration: Declaration, artifactId: String, groupId: String)(implicit module:Module, moduleNode: Node): Node = {
    val groupTupleOpt = cache(groupId)

    // group, artifact, declaration needs to be created
    if (groupTupleOpt.isEmpty) {
      val groupNode = createNode(Labels.Group, Map("groupId" -> groupId))
      val artifactNode = createNode(Labels.Artifact, Map("artifactId" -> artifactId))
      val declarationNode = createDeclarationNode(declaration)

      cache.put(groupId, groupNode).put(artifactId, artifactNode).put(declaration.declarationId, declarationNode)

      groupNode.createRelationshipTo(artifactNode, Relationships.GROUP_ARTIFACT)
      artifactNode.createRelationshipTo(declarationNode, Relationships.ARTIFACT_DECLARATION)
      if (module.artifactId == artifactId && module.groupId == groupId)
        moduleNode.createRelationshipTo(declarationNode, Relationships.DECLARES)
      declarationNode
    }
    else {
      val (groupNode, artifactCache) = groupTupleOpt.get

      val artifactTupleOpt = artifactCache(artifactId)

      // artifact, declaration needs to be created
      if (artifactTupleOpt.isEmpty) {
        val artifactNode = createNode(Labels.Artifact, Map("artifactId" -> artifactId))
        val declarationNode = createDeclarationNode(declaration)

        artifactCache.put(artifactId, artifactNode).put(declaration.declarationId, declarationNode)

        groupNode.createRelationshipTo(artifactNode, Relationships.GROUP_ARTIFACT)
        artifactNode.createRelationshipTo(declarationNode, Relationships.ARTIFACT_DECLARATION)
        if (module.artifactId == artifactId && module.groupId == groupId)
          moduleNode.createRelationshipTo(declarationNode, Relationships.DECLARES)
        declarationNode
      }
      else {
        val (artifactNode, declarationCache) = artifactTupleOpt.get

        val declarationTuple = declarationCache(declaration.declarationId).getOrElse({
          val declarationNode = createDeclarationNode(declaration)

          artifactNode.createRelationshipTo(declarationNode, Relationships.ARTIFACT_DECLARATION)
          if (module.artifactId == artifactId && module.groupId == groupId)
            moduleNode.createRelationshipTo(declarationNode, Relationships.DECLARES)
          (declarationNode, declarationCache.put(declaration.declarationId, declarationNode))
        })
        declarationTuple._1
      }
    }
  }

  // For the purpose to map typereferece, which with declaration id, that does not correspond to any declarationId in module.declarations
  def createUnknownDeclarationNode(): Node = {
    val declaration: Declaration = Utils.createUnknownDeclaration()
    val groupId = Utils.UNKNOWN_GROUP_ID
    val artifactId = Utils.UNKNOWN_ARTIFACT_ID

    val groupNode = createNode(Labels.Group, Map("groupId" -> groupId))
    val artifactNode = createNode(Labels.Artifact, Map("artifactId" -> artifactId))
    val declarationNode = createDeclarationNode(declaration)

    cache.put(groupId, groupNode).put(artifactId, artifactNode).put(declaration.declarationId, declarationNode)

    groupNode.createRelationshipTo(artifactNode, Relationships.GROUP_ARTIFACT)
    artifactNode.createRelationshipTo(declarationNode, Relationships.ARTIFACT_DECLARATION)

    declarationNode
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