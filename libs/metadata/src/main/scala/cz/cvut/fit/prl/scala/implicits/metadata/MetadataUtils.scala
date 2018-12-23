package cz.cvut.fit.prl.scala.implicits.metadata

object MetadataUtils {
  def moduleId(organization: String, name: String, version: String): String =
    Seq(organization, name, version).mkString(":")
  def moduleId(organization: String, name: String, version: String, platform: String): String =
    moduleId(organization, name, version) + ":" + platform
}
