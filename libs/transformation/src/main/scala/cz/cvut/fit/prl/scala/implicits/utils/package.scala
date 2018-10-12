package cz.cvut.fit.prl.scala.implicits

import cz.cvut.fit.prl.scala.implicits.{model => m}

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

  // TODO: do we need this?
  implicit class XtensionPosition(that: Position) {
    def toRange: s.Range =
      s.Range(that.startLine, that.startColumn, that.endLine, that.endColumn)
  }

  implicit class XtensionRange(that: s.Range) {
    def toLocal: m.Position =
      m.Position(that.startLine, that.startCharacter, that.endLine, that.endCharacter)
  }

  // TODO: do we need this?
  implicit def position2Range(that: Position): s.Range = that.toRange
  implicit def position2Local(that: Position): m.Position =
    m.Position(that.startLine, that.startColumn, that.endLine, that.endColumn)
  implicit def range2Local(that: s.Range): m.Position = that.toLocal

  implicit def language2language(that: s.Language): m.Language = that match {
    case s.Language.UNKNOWN_LANGUAGE => m.Language.UNKNOWN_LANGUAGE
    case s.Language.SCALA => m.Language.SCALA
    case s.Language.JAVA => m.Language.JAVA
    case _ => throw new Exception(s"SDB Language `$that` is not supported yet")
  }

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
