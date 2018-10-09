package cz.cvut.fit.prl.scala.implicits

import scala.language.implicitConversions
import scala.meta.internal.{semanticdb => s}
import scala.meta.inputs.Position
import scala.util.{Failure, Success, Try}

package object utils {
  implicit class XtensionOptions[+A](that: Option[A]) {
    def getOrThrow(e: Throwable): A = that match {
      case Some(x) => x
      case None => throw e
    }
  }

  implicit class XtensionPosition(that: Position) {
    def toRange: s.Range =
      s.Range(that.startLine, that.startColumn, that.endLine, that.endColumn)
  }

  implicit def position2Range(that: Position): s.Range = that.toRange

  implicit class XtensionTraversable[T](that: Traversable[T]) {
    def mkStringOpt(start: String, sep: String, end: String): String =
      if (that.isEmpty) "" else that.mkString(start, sep, end)
  }

  implicit class XtensionTraversableTry[T](that: Traversable[Try[T]]) {
    def split(): (List[T], List[Throwable]) =
      that.foldLeft((List[T](), List[Throwable]())) { case ((succ, fail), t) =>
        t match {
          case Success(x) => (x :: succ, fail)
          case Failure(x) => (succ, x :: fail)
        }
      }
  }

}
