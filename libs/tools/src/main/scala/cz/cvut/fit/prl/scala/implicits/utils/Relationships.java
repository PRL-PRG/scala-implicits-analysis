package cz.cvut.fit.prl.scala.implicits.utils;

import org.neo4j.graphdb.RelationshipType;

public enum Relationships implements RelationshipType {
    LOCATED_IN,
    HAS_DECLARATION,
    HAS_ARTIFACT,
    HAS_VERSION,
    HAS_MODULE,
    HAS_CALLSITE,
    HAS_PATH
}
