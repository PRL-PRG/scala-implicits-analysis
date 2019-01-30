package cz.cvut.fit.prl.scala.implicits.model

class TypeResolverStub(table: Map[String, Declaration]) extends TypeResolver {
  override def resolveType(tpe: Type): Declaration = table(tpe.declarationRef)
}

object TypeResolverStub {
  def apply(xs: Declaration*): TypeResolverStub =
    new TypeResolverStub(xs.map(x => x.fqn -> x).toMap)
}
