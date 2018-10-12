package cz.cvut.fit.prl.scala.implicits

package object model {

  implicit class XtensionParameterList(that: ParameterList) {
    def isImplicit: Boolean = that.parameters.exists(_.isImplicit)
  }

}
