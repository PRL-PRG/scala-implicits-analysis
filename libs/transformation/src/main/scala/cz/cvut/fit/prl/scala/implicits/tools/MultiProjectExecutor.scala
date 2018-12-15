package cz.cvut.fit.prl.scala.implicits.tools

import java.util.concurrent._

import better.files._
import cats.Monoid
import cats.syntax.monoid._
import cz.cvut.fit.prl.scala.implicits.Constants

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

class MultiProjectExecutor[R, S: Monoid](producer: File => R, consumer: (String, Try[R]) => S) {
  case class Status(status: S, failures: Seq[(String, Throwable)])

  def run(projectsFile: File, threads: Int): Status = {
    run(projectsFile.lines.map(x => Constants.ProjectsDirname / x), threads)
  }

  def run(projectsPaths: Traversable[File], threads: Int): Status = {
    val finishedProjects: BlockingQueue[(String, Try[R])] = new LinkedBlockingQueue()
    val consumerPool = Executors.newSingleThreadExecutor()
    val producerPool = Executors.newWorkStealingPool(threads)
    val latch = new CountDownLatch(projectsPaths.size)

    val driver = consumerPool.submit(new Callable[Status] {
      override def call(): Status = {
        var jobStats = Monoid[S].empty
        val failures = mutable.Buffer[(String, Throwable)]()

        while (latch.getCount > 0) {
          val (projectId, taskResult) = finishedProjects.take()
          try {
            val progress =
              s"[${projectsPaths.size - latch.getCount + 1}/${projectsPaths.size}]: $projectId"

            val stats = consumer(projectId, taskResult)
            jobStats = jobStats |+| stats

            taskResult match {
              case Success(value) =>
                println(s"$progress: Success $stats")
              case Failure(exception) =>
                println(
                  s"$progress: Failure: ${exception.getClass.getSimpleName}: ${exception.getMessage}"
                )
                exception.printStackTrace()
                failures += projectId -> exception
            }
          } finally {
            latch.countDown()
          }
        }

        Status(jobStats, failures)
      }
    })

    val startTime = System.currentTimeMillis()

    println(s"Processing ${projectsPaths.size} projects with $threads thread(s) ...")

    projectsPaths.foreach { path =>
      producerPool.submit(new Runnable {
        override def run(): Unit = finishedProjects.put(path.name -> Try(producer(path)))
      })
    }

    latch.await()
    consumerPool.shutdown()
    producerPool.shutdown()

    val stats = driver.get()

    println("Job took: " + (System.currentTimeMillis() - startTime) + " ms")
    println(s"Status: ${stats.status}")

    if (stats.failures.nonEmpty) {
      println("Failures:")
      stats.failures.foreach {
        case (projectId, exception) =>
          println(s"- $projectId -- ${exception.getClass.getSimpleName}: ${exception.getMessage}")
      }
    }

    stats
  }
}
