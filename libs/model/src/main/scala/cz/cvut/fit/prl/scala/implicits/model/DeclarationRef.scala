package cz.cvut.fit.prl.scala.implicits.model

case class DeclarationRef(moduleId: String, declarationFqn: String) {
  def declaration(implicit resolver: TypeResolver): Declaration =
    resolver.resolveType(this)
}
