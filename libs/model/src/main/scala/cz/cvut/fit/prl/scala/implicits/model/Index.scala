package cz.cvut.fit.prl.scala.implicits.model


trait Index extends ModuleResolver with DeclarationResolver {
  def projects: Iterable[Project]
  def modules: Iterable[Module]
  def implicitDeclarations: Iterable[Declaration]
  def implicitCallSites: Iterable[CallSite]
}

