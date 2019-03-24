package cz.cvut.fit.prl.scala.implicits.model

trait DeclarationResolver {
  def resolveDeclaration(ref: DeclarationRef): Declaration
  def resolveDeclaration(moduleId: String, declarationId: String): Declaration =
    resolveDeclaration(DeclarationRef(moduleId, declarationId))
}

trait ModuleResolver {
  def module(moduleId: String): Module
  def project(projectId: String): Project
}