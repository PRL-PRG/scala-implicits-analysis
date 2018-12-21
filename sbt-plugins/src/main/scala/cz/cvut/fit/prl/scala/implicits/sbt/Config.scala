package cz.cvut.fit.prl.scala.implicits.sbt

object Config {
  val ScalametaVersion: String = System.getenv.getOrDefault("SCALAMETA_VERSION", "4.1.0")
  val SemanticdbScalacOptions: Seq[String] = Seq(
    "-Yrangepos",
    "-P:semanticdb:text:on",
    "-P:semanticdb:symbols:all",
    "-P:semanticdb:synthetics:on",
    "-P:semanticdb:diagnostics:off",
    "-P:semanticdb:failures:warning"
  )

  private[this] val supported211 = Seq(9, 10, 11, 12).map(x => s"2.11.$x")
  private[this] val supported212 = Seq(3, 4, 5, 6, 7, 8).map(x => s"2.12.$x")

  private[this] val default210 = "2.11.12"
  private[this] val default211 = "2.11.12"
  private[this] val default212 = "2.12.7"

  def updateVersion(maj: Long, min: Long, fullVersion: String): String =
    updateVersion(maj.toInt, min.toInt, fullVersion)

  def updateVersion(maj: Int, min: Int, fullVersion: String): String = (maj, min) match {
    case (2, 10) => default210
    case (2, 11) => supported211.find(_ == fullVersion).getOrElse(default211)
    case (2, 12) => supported212.find(_ == fullVersion).getOrElse(default212)
    case _ => throw new IllegalArgumentException(s"Unsupported Scala version $fullVersion")
  }
}
