package cz.cvut.fit.prl.scala.implicits.utils;

import org.neo4j.graphdb.RelationshipType;

public enum Relationships implements RelationshipType {
    LOCATED_IN,
    DECLARES,
    DECLARED_BY,
    HAS_VERSION,
    HAS_MODULE,
    HAS_CALLSITE,
    GROUP_ARTIFACT,
    ARTIFACT_DECLARATION,
    HAS_PATH
}
