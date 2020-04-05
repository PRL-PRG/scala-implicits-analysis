package cz.cvut.fit.prl.scala.implicits.utils;


import org.neo4j.graphdb.Label;

public enum Labels implements Label {
    Project,
    Path,
    SourcePath,
    Declaration,
    LibraryGroup,
    Library,
    Classpath,
    Module,
    CallSite,
    Artifact,
    Group,
    Language,
    Access,
    DeclarationType,
    Signature,
    SignatureType,
    TypeReference,
    Parameter,
    ParameterList,
    Implicit,
    ImplicitType
}
