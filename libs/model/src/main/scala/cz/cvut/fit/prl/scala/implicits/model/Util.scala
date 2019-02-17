package cz.cvut.fit.prl.scala.implicits.model
import com.typesafe.scalalogging.Logger

object Util {
  def timedTask[T](name: String, thunk: => T)(implicit logger: Logger): T = {
    logger.debug(name + "...")

    val time = System.currentTimeMillis()

    try {
      val result = thunk
      val elapsed = System.currentTimeMillis() - time
      logger.debug(name + " in " + elapsed)
      result
    } catch {
      case e: Throwable =>
        logger.warn(name + " FAILED")
        throw e
    }
  }

}
