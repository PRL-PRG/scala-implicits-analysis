package cz.cvut.fit.prl.scala.implicits.utils
import java.io.PrintWriter
import java.util.concurrent.ForkJoinPool
import java.util.concurrent.atomic.AtomicInteger

import better.files._
import cats.Monoid
import cats.syntax.monoid._
import cz.cvut.fit.prl.scala.implicits.utils.MultiProjectExecutor.Result

import scala.collection.parallel.ForkJoinTaskSupport
import scala.util.{Failure, Success, Try}

trait Reporting {
  def writeReport(writer: PrintWriter): Unit
  def status: String
}

object MultiProjectExecutor {
  case class Result[R: Monoid](status: R, failures: List[Throwable]) {

    def printSummary() = {
      println()

      if (failures.nonEmpty) {
        println("Failure summary:")
        failures
          .groupCount(_.getMessage)
          .foreach {
            case (problem, size) => println(s"\t$size -- $problem")
          }
        println()
      }

      status match {
        case x: Reporting =>
          x.writeReport(new PrintWriter(System.out))
        case x => println(x)
      }
      System.out.flush()
    }
  }
}

class MultiProjectExecutor[R: Monoid](task: File => R, threads: Int = 5) {

  def run(projectsPaths: List[File]): Result[R] = {
    val completed = new AtomicInteger(0)

    def printProgress(path: File, result: Try[R]): Unit = synchronized {
      val progress = s"${path.name} [${completed.incrementAndGet()}/${projectsPaths.size}]"
      result match {
        case Success(x: Reporting) =>
          println(s"Success $progress : ${x.status}")
        case Success(x) =>
          println(s"Success $progress : $x")
        case Failure(e) =>
          println(s"Failure $progress : ${e.getMessage}")
      }
    }

    def runOne(path: File): Try[R] = {
      val result = Try(task(path))
      printProgress(path, result)
      result
    }

    println(s"Processing ${projectsPaths.size} projects with $threads thread(s) ...")

    val parProjectsPaths = projectsPaths.par
    parProjectsPaths.tasksupport = new ForkJoinTaskSupport(new ForkJoinPool(threads))

    val e = Result(implicitly[Monoid[R]].empty, Nil)

    val xx = parProjectsPaths
      .map(x => x -> runOne(x))

    val taskResult = xx.seq
      .foldLeft(e) {
        case (result, combined) =>
          combined match {
            case (_, Success(x)) =>
              result.copy(status = result.status |+| x)
            case (_, Failure(e)) => result.copy(failures = e :: result.failures)
          }
      }

    taskResult
  }
}
