package cz.cvut.fit.prl.scala.implicits.utils

import cz.cvut.fit.prl.scala.implicits.model.ClasspathEntry

import scala.meta.io.Classpath

object Libraries {
  val JvmVersion: String = System.getProperty("java.version")
  val JvmBootClasspath: Classpath = {
    val prop =
      sys.props
        .collectFirst { case (k, v) if k.endsWith(".boot.class.path") => v }
        .getOrThrow(new Exception("Unable to find java boot classpath (java.boot.class.path)"))
    val entries = Classpath(prop).entries.filter(_.isFile)
    Classpath(entries)
  }

  val JvmBootModelClasspath: Seq[ClasspathEntry] =
    JvmBootClasspath.entries.map(x =>
      ClasspathEntry(
        x.toString(),
        "JDK",
        "JDK",
        JvmVersion,
        "compile",
        internal = false,
        managed = true
      )
    )
}
