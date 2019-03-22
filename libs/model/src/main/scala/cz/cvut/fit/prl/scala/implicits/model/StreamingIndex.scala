package cz.cvut.fit.prl.scala.implicits.model

import java.io.InputStream

import better.files.File
import cz.cvut.fit.prl.scala.implicits.model.Util._

class StreamingIndex(dataFile: File) extends Index {
  private var currentIndex: Index = _

  class ProjectIterator[A](open: Index => Iterator[A]) extends Iterator[A] {
    lazy val input: InputStream = dataFile.newInputStream
    lazy val projectIterator: Iterator[Project] = Project.streamFrom(input).iterator

    var currentIterator: Iterator[A] = Seq.empty.iterator

    override def hasNext: Boolean = {
      if (currentIterator.hasNext) {
        true
      } else if (projectIterator.hasNext) {
        currentIndex = ProjectIndex(projectIterator.next())
        currentIterator = open(currentIndex)
        hasNext
      } else {
        input.close()
        false
      }
    }

    override def next(): A = currentIterator.next()
  }

  override def implicitDeclarations: Iterable[Declaration] = new Iterable[Declaration] {
    def iterator = new ProjectIterator[Declaration](_.implicitDeclarations.iterator)
  }

  override def implicitCallSites: Iterable[CallSite] = new Iterable[CallSite] {
    override def iterator: Iterator[CallSite] = {
      val input = dataFile.newInputStream
      val projectIterator = Project.streamFrom(input).iterator
      var currentIterator: Iterator[CallSite] = Seq.empty.iterator

      new Iterator[CallSite] {
        override def hasNext: Boolean = {
          if (currentIterator.hasNext) {
            true
          } else if (projectIterator.hasNext) {
            currentIndex = ProjectIndex(projectIterator.next())
            currentIterator = currentIndex.implicitCallSites.iterator
            hasNext
          } else {
            input.close()
            false
          }
        }

        override def next(): CallSite = currentIterator.next()
      }
    }
  }

  override def projects: Iterable[Project] = currentIndex.projects
  override def modules: Iterable[Module] = currentIndex.modules
  override def project(projectId: String): Project = currentIndex.project(projectId)
  override def module(moduleId: String): Module = currentIndex.module(moduleId)
  override def resolveDeclaration(ref: DeclarationRef): Declaration = currentIndex.resolveDeclaration(ref)
}
