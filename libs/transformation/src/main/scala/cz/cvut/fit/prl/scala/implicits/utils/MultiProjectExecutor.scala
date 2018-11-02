package cz.cvut.fit.prl.scala.implicits.utils
import java.io.PrintWriter
import java.util.concurrent.{CountDownLatch, Executors}

import better.files._
import cats.Monoid
import cats.syntax.monoid._
import cz.cvut.fit.prl.scala.implicits.utils.MultiProjectExecutor.Result

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}
import scala.util.{Failure, Success, Try}

trait Reporting {
  def writeReport(writer: PrintWriter): Unit
  def status: String
}

object MultiProjectExecutor {
  case class Result[R](status: R, failures: List[Throwable]) {

    def printSummary(): Unit = {
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

class MultiProjectExecutor[R](task: File => R, threads: Int)(implicit ev: Monoid[R]) {

  def run(projectsPaths: List[File]): Result[R] = {
    var completed = 0
    var summary = Result(ev.empty, Nil)
    val latch = new CountDownLatch(projectsPaths.size)
    implicit val ec: ExecutionContextExecutor =
      ExecutionContext.fromExecutor(Executors.newWorkStealingPool(threads))

    def completeOne(path: File, result: Try[R]): Unit = synchronized {
      completed = completed + 1
      val progress = s"${path.name} [$completed/${projectsPaths.size}]"
      result match {
        case Success(x) =>
          val status = x match {
            case reporting: Reporting => reporting.status
            case _                    => x.toString
          }
          println(s"Success $progress : $status")
          summary = summary.copy(status = summary.status |+| x)
        case Failure(e) =>
          println(s"Failure $progress : ${e.getMessage}")
          summary = summary.copy(failures = e :: summary.failures)
      }
    }

    def runOne(path: File): Future[R] = Future {
      val result = Try(task(path))
      completeOne(path, result)
      latch.countDown()
      result.get
    }

    val startTime = System.currentTimeMillis()

    println(s"Processing ${projectsPaths.size} projects with $threads thread(s) ...")

    projectsPaths.foreach(runOne)

    latch.await()

    println("Job took: " + (System.currentTimeMillis() - startTime) + " ms")

    summary
  }
}
