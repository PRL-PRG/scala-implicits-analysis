package cz.cvut.fit.prl.scala.implicits.model

trait DeclarationResolver {
  def resolveDeclaration(ref: String): Declaration
}
