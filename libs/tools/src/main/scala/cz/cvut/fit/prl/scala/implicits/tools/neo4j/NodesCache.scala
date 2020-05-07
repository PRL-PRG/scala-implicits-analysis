package cz.cvut.fit.prl.scala.implicits.tools.neo4j

import org.neo4j.graphdb.Node

import scala.collection.mutable

// map[groupId, (groupNode, Map[artifactId, (artifactNode, Map[declarationId, (declarationNode, Map[typeReferenceExpression, typeReferenceNode)])])
class NodesCache(cache: mutable.Map[String,(Node, ArtifactCache)] ) {
  def apply(key: String): Option[(Node, ArtifactCache)] = cache.get(key)
  def put(key: String, value: Node): ArtifactCache = {
    val artifactCache = ArtifactCache()
    cache.put(key, (value, artifactCache))
    artifactCache
  }
}

object NodesCache {
  def apply(): NodesCache = new NodesCache(new mutable.HashMap[String, (Node, ArtifactCache)])
}


class TypeReferenceCache(cache: mutable.Map[String, Node]) {
  def apply(key: String): Option[Node] = cache.get(key)
  def put(key: String, value: Node): Unit = cache.put(key, value)
}
object TypeReferenceCache {
  def apply() = new TypeReferenceCache(new mutable.HashMap[String,Node]())
}


class DeclarationCache(cache: mutable.Map[String, (Node, TypeReferenceCache)]) {
  def apply(key: String): Option[(Node, TypeReferenceCache)] = cache.get(key)
  def put(key: String, value: Node): TypeReferenceCache = {
    val typeReferenceCache = TypeReferenceCache()
    cache.put(key, (value, typeReferenceCache))
    typeReferenceCache
  }
}
object DeclarationCache {
  def apply(): DeclarationCache = new DeclarationCache(new mutable.HashMap[String, (Node, TypeReferenceCache)])
}


class ArtifactCache(cache: mutable.Map[String, (Node, DeclarationCache)]) {
  def apply(key: String): Option[(Node, DeclarationCache)] = cache.get(key)
  def put(key: String, value: Node): DeclarationCache = {
    val declarationCache = DeclarationCache()
    cache.put(key, (value, declarationCache))
    declarationCache
  }
}
object ArtifactCache {
  def apply(): ArtifactCache = new ArtifactCache(new mutable.HashMap[String, (Node, DeclarationCache)])
}