package cz.cvut.fit.prl.scala.implicits.model

case class DeclarationRef(moduleId: String, declarationId: String) {
  def declaration(implicit resolver: DeclarationResolver): Declaration =
    resolver.resolveDeclaration(this)
}
