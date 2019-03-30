package cz.cvut.fit.prl.scala.implicits.model

case class Library(groupId: String, artifactId: String, version: String) {
  override def toString: String = s"$groupId:$artifactId:$version"
}
