package cz.cvut.fit.prl.scala.implicits.model

trait DeclarationResolver {
  def resolveDeclaration(ref: DeclarationRef): Declaration
}

trait ModuleResolver {
  def module(moduleId: String): Module
  def project(projectId: String): Project
}