package cz.cvut.fit.prl.scala.implicits

import cz.cvut.fit.prl.scala.implicits.{model => m}

object TestDirectQuery extends App {
  val util = new SemanticdbSuite {}
  val code =
    """
      |object InferredApplyCall {
      |  1 -> 2
      |}
    """.stripMargin

  val db = util.computeDatabaseFromSnippet(code)
  val transformer = new CallSiteExtractor(db, util.symtab)

  pprint.pprintln(transformer.declarations, height = Integer.MAX_VALUE)
  transformer.failures.foreach(_.printStackTrace())
}
