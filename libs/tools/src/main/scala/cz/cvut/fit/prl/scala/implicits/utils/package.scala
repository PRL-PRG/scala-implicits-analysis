package cz.cvut.fit.prl.scala.implicits

import java.io.PrintWriter

import cz.cvut.fit.prl.scala.implicits.extractor.SymbolResolver
import cz.cvut.fit.prl.scala.implicits.metadata.Module
import cz.cvut.fit.prl.scala.implicits.{model => m}

import scala.collection.mutable
import scala.language.implicitConversions
import scala.meta.inputs.Position
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.{semanticdb => s}
import scala.meta.io.{AbsolutePath, Classpath}
import scala.util.{Failure, Success, Try}

package object utils {
  implicit class XtensionSynthetic(that: s.Synthetic) {
    def toStringLegacy: String = LegacySyntheticPrinter.toLegacy(that)
  }

  implicit class XtensionSymbolInformation(that: s.SymbolInformation) {
    def parent(implicit resolver: SymbolResolver, db: s.TextDocument): s.SymbolInformation =
      resolver.resolveSymbol(that.symbol.owner).symbolInfo
  }

  implicit class XtensionSDBType(that: s.Type) {
    def isTopOrBottom: Boolean = that match {
      case s.TypeRef(_, "scala/AnyRef#", _) => true
      case s.TypeRef(_, "scala/Any#", _) => true
      case s.TypeRef(_, "scala/Nothing#", _) => true
      case _ => false
    }
  }

  implicit class XtensionAnyRef(that: AnyRef) {
    def prettyPrint(): Unit = pprint.pprintln(that, height = Integer.MAX_VALUE)
  }

  implicit class XtensionPosition(that: Position) {
    def toRange: s.Range =
      s.Range(that.startLine, that.startColumn, that.endLine, that.endColumn)
  }

  implicit class XtensionRange(that: s.Range) {
    def toPos: m.Position =
      m.Position(that.startLine, that.startCharacter, that.endLine, that.endCharacter)
  }

  implicit class XtensionModuleClasspath(that: Module) {
    def classpath: Classpath = Classpath(that.output.map(AbsolutePath(_)).toList)
  }

  implicit class XtensionOptions[+A](that: Option[A]) {
    def getOrThrow(e: => Throwable): A = that match {
      case Some(x) => x
      case None => throw e
    }
  }

  implicit class XtensionTraversable[T](that: Traversable[T]) {
    def mkStringOpt(start: String, sep: String, end: String): String =
      if (that.isEmpty) "" else that.mkString(start, sep, end)
  }

  implicit class XtensionTraversableTry[T](that: Traversable[Try[T]]) {
    def split(): (List[T], List[Throwable]) =
      that.foldLeft((List[T](), List[Throwable]())) {
        case ((succ, fail), t) =>
          t match {
            case Success(x) => (x :: succ, fail)
            case Failure(x) => (succ, x :: fail)
          }
      }
  }

  implicit class XtensionTranverable[T](that: Traversable[T]) {
    def printGroups[K](f: T => K, out: PrintWriter = new PrintWriter(System.out)): Unit = {
      that.groupCount(f).foreach {
        case (problem, size) =>
          out.println(s"\t$size -- $problem")
      }
      out.flush()
    }

    def groupCount[K](f: T => K): Seq[(K, Int)] = {
      that.groupBy(f).mapValues(x => x.size).toSeq.sortBy(-_._2)
    }
  }

  implicit class XtensionMap[A, B](that: mutable.Map[A, B]) {
    def updateValue(key: A, fun: B => B): B = {
      val n = fun(that(key))
      that(key) = n
      n
    }
  }
}
