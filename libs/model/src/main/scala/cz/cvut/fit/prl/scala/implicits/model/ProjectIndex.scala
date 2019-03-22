package cz.cvut.fit.prl.scala.implicits.model

case class ProjectIndex(project: Project) extends Index {
  override def projects: Iterable[Project] = Seq(project)

  override def modules: Iterable[Module] = project.modules.values

  override def implicitDeclarations: Iterable[Declaration] =
    project.implicitDeclarations

  override def implicitCallSites: Iterable[CallSite] =
    project.implicitCallSites

  override def resolveDeclaration(ref: DeclarationRef): Declaration =
    project.modules(ref.moduleId).declarations(ref.declarationId)

  override def module(moduleId: String): Module = project.modules(moduleId)

  override def project(projectId: String): Project = {
    if (projectId == project.projectId) {
      project
    } else {
      throw new NoSuchElementException(
        s"This is a single project ${project.projectId} index, unable to resolve $projectId")
    }
  }
}