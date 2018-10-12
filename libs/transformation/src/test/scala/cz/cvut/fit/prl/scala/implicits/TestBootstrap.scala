package cz.cvut.fit.prl.scala.implicits

import cz.cvut.fit.prl.scala.implicits.{model => m}

import scala.meta.internal.semanticdb.SymbolInformation.Kind.METHOD

object TestBootstrap extends App {
  val util = new SemanticdbSuite {}
  val code =
    """
      |object InferredApplyCall {
      |  Seq(1)
      |}
    """.stripMargin

  val db = util.computeDatabaseFromSnippet(code)
  val transformer = new CallSiteExtractor(db, util.symtab)

  pprint.pprintln(transformer.declarations.values.collect{case x: m.MethodDeclaration => x}, height = Integer.MAX_VALUE)
  transformer.failures.foreach(_.printStackTrace())
}
