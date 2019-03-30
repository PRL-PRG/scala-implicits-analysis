package cz.cvut.fit.prl.scala.implicits.model

trait DeclarationResolver {
  def resolveDeclaration(ref: DeclarationRef): Declaration
  def resolveDeclaration(moduleId: String, declarationId: String): Declaration =
    resolveDeclaration(DeclarationRef(moduleId, declarationId))
}

trait LocalDeclarationResolver extends DeclarationResolver {
  def resolveDeclaration(declarationId: String): Declaration
  override def resolveDeclaration(ref: DeclarationRef): Declaration =
    resolveDeclaration(ref.declarationId)
}

trait ModuleResolver {
  def module(moduleId: String): Module
  def project(projectId: String): Project
}
