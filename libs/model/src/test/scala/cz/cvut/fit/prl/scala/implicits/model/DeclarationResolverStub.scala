package cz.cvut.fit.prl.scala.implicits.model

class DeclarationResolverStub(table: Map[String, Declaration]) extends DeclarationResolver {
  override def resolveDeclaration(ref: DeclarationRef): Declaration = table(ref.declarationId)
}

object DeclarationResolverStub {
  def apply(xs: Declaration*): DeclarationResolverStub =
    new DeclarationResolverStub(xs.map(x => x.declarationId -> x).toMap)
}
