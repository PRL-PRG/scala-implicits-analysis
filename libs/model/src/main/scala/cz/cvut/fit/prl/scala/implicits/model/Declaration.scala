package cz.cvut.fit.prl.scala.implicits.model

trait Declaration {
  def ref: String = s"${location.path}:$fqn"
  def symbol: Symbol
  def fqn: String = symbol.fqn
  def name: String = symbol.name
  def location: Location = symbol.location
  def isImplicit: Boolean = symbol.isImplicit
  def language: Language = symbol.language
}
