package cz.cvut.fit.prl.scala.implicits.tools.neo4j

import org.neo4j.graphdb.{Node, Transaction}

import scala.collection.mutable

// map[groupId, (groupNode, Map[artifactId, (artifactNode, Map[declarationId, (declarationNode, Map[typeReferenceExpression, typeReferenceNode)])])
class NodesCache(val cache: mutable.Map[String,(Long, ArtifactCache)] ) {
  def apply(key: String)(implicit transaction: Transaction): Option[(Node, ArtifactCache)] = NodeMapper(cache.get(key))
  def put(key: String, node: Node): ArtifactCache = {
    val artifactCache = ArtifactCache()
    cache.put(key, (node.getId, artifactCache))
    artifactCache
  }
  def getDeclarationTuple(groupId: String, artifactId: String, declarationId: String)(implicit transaction: Transaction): (Node, TypeReferenceCache) =
    NodeMapper(cache(groupId)._2.cache(artifactId)._2.cache(declarationId))
}

object NodesCache {
  def apply(): NodesCache = new NodesCache(new mutable.HashMap[String, (Long, ArtifactCache)])
}


class TypeReferenceCache(val cache: mutable.Map[String, Long]) {
  def apply(key: String)(implicit transaction: Transaction): Option[Node] = cache.get(key).map(transaction.getNodeById)
  def put(key: String, node: Node): Unit = cache.put(key, node.getId)
}
object TypeReferenceCache {
  def apply() = new TypeReferenceCache(new mutable.HashMap[String,Long]())
}


class DeclarationCache(val cache: mutable.Map[String, (Long, TypeReferenceCache)]) {
  def apply(key: String)(implicit  transaction: Transaction): Option[(Node, TypeReferenceCache)] = NodeMapper(cache.get(key))
  def put(key: String, node: Node): TypeReferenceCache = {
    val typeReferenceCache = TypeReferenceCache()
    cache.put(key, (node.getId, typeReferenceCache))
    typeReferenceCache
  }
}
object DeclarationCache {
  def apply(): DeclarationCache = new DeclarationCache(new mutable.HashMap[String, (Long, TypeReferenceCache)])
}


class ArtifactCache(val cache: mutable.Map[String, (Long, DeclarationCache)]) {
  def apply(key: String)(implicit transaction: Transaction): Option[(Node, DeclarationCache)] = NodeMapper(cache.get(key))
  def put(key: String, node: Node): DeclarationCache = {
    val declarationCache = DeclarationCache()
    cache.put(key, (node.getId, declarationCache))
    declarationCache
  }
}

object ArtifactCache {
  def apply(): ArtifactCache = new ArtifactCache(new mutable.HashMap[String, (Long, DeclarationCache)])
}

object NodeMapper {
  def apply[CacheType](cacheTupleOpt : Option[(Long, CacheType)])(implicit transaction: Transaction):Option[(Node, CacheType)] = {
    cacheTupleOpt match {
      case Some(cacheTuple) => Some((transaction.getNodeById(cacheTuple._1), cacheTuple._2 ))
      case None => None
    }
  }
  def apply[CacheType](cacheTuple : (Long, CacheType))(implicit transaction: Transaction):(Node, CacheType) = {
    (transaction.getNodeById(cacheTuple._1), cacheTuple._2)
  }
}
