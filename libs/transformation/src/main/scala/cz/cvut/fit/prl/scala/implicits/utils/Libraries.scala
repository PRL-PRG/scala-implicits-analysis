package cz.cvut.fit.prl.scala.implicits.utils

import scala.meta.io.Classpath

object Libraries {
  val JvmBootClasspath = {
    val prop =
      sys.props
        .collectFirst { case (k, v) if k.endsWith(".boot.class.path") => v }
        .getOrThrow(new Exception("Unable to find java boot classpath (java.boot.class.path)"))
    val entries = Classpath(prop).entries.filter(_.isFile)
    Classpath(entries)
  }

}
