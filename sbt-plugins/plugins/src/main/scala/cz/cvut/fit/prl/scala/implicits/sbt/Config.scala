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

  private[this] val VersionMapping: Map[(Long, Long), String] = Map(
    (2L -> 11L) -> "2.11.12",
    (2L -> 12L) -> "2.12.6"
  )

  val VersionMapping_sbt_1_0: Map[(Long, Long), String] = VersionMapping

  // It is Int => Int unlike in sbt >= 1.0
  val VersionMapping_sbt_0_13: Map[(Int, Int), String] =
    VersionMapping.map {
      case ((maj, min), target) => ((maj.toInt, min.toInt), target)
    }
}
