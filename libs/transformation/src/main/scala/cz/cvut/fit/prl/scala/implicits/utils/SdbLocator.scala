package cz.cvut.fit.prl.scala.implicits.utils

import java.nio.file._
import java.util.jar._

import scala.meta.internal.semanticdb.TextDocuments

// Based on scala.meta.internal.semanticdb.Locator
class SdbLocator(path: Path, filter: Path => Boolean, options: Seq[FileVisitOption]) {
  def this(path: Path) = this(path, _ => true, Seq())

  def exclude(names: Seq[String]): SdbLocator =
    new SdbLocator(
      path,
      x => filter(x) && !names.contains(x.toFile.getName),
      options
    )

  def options(options: FileVisitOption*): SdbLocator =
    new SdbLocator(path, filter, this.options ++ options)

  def run(fn: (Path, TextDocuments) => Unit): Unit = {
    if (Files.exists(path)) {
      if (Files.isDirectory(path)) {
        Files
          .walk(path, options: _*)
          .filter(_.toString.endsWith(".semanticdb"))
          .forEach { path =>
            val stream = Files.newInputStream(path)
            try fn(path, TextDocuments.parseFrom(stream))
            finally stream.close()
          }
      } else {
        if (path.toString.endsWith(".jar")) {
          // NOTE: Can't use nio.Files.walk because nio.FileSystems
          // is not supported on Scala Native.
          val jar = new JarFile(path.toFile)
          val buf = List.newBuilder[JarEntry]
          val jarIt = jar.entries()
          while (jarIt.hasMoreElements) {
            val jarEntry = jarIt.nextElement()
            if (jarEntry.getName.endsWith(".semanticdb")) {
              buf += jarEntry
            }
          }
          val jarEntries = buf.result.sortBy(_.getName.toLowerCase)
          jarEntries.foreach { jarEntry =>
            val path = Paths.get(jarEntry.getName)
            val stream = jar.getInputStream(jarEntry)
            try fn(path, TextDocuments.parseFrom(stream))
            finally stream.close()
          }
          val manifest = jar.getManifest
          if (manifest != null) {
            val classpathAttr = manifest.getMainAttributes.getValue("Class-Path")
            if (classpathAttr != null) {
              classpathAttr.split(" ").foreach { relativePath =>
                val parentPath = path.toAbsolutePath.getParent
                new SdbLocator(parentPath.resolve(relativePath)).run(fn)
              }
            }
          }
        } else if (path.toString.endsWith(".semanticdb")) {
          val stream = Files.newInputStream(path)
          try fn(path, TextDocuments.parseFrom(stream))
          finally stream.close()
        } else {
          ()
        }
      }
    } else {
      ()
    }
  }
}
