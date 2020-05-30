package cz.cvut.fit.prl.scala.implicits.tools.neo4j

import cz.cvut.fit.prl.scala.implicits.model.Declaration.Kind.TYPE
import cz.cvut.fit.prl.scala.implicits.model.Language.UNKNOWN_LANGUAGE
import cz.cvut.fit.prl.scala.implicits.model.{ClasspathEntry, Declaration, Location, Module, SourcepathEntry, TypeRef, TypeSignature}

object Utils {

  val UNKNOWN_GROUP_ID = "UNKNOWN_GROUP_ID"
  val UNKNOWN_ARTIFACT_ID = "UNKNOWN_ARTIFACT_ID"


  def createUnknownDeclaration(): Declaration = {
    Declaration("UNKNOWN","", TYPE,0,"",Declaration.Access.NOT_SPECIFIED,Location("","",None),UNKNOWN_LANGUAGE, Seq.empty, TypeSignature())
  }
}
