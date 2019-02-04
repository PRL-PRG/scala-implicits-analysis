package cz.cvut.fit.prl.scala.implicits.model

trait TypeResolver {
  def resolveType(ref: DeclarationRef): Declaration
}
