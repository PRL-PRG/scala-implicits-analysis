package cz.cvut.fit.prl.scala.implicits.model

import java.io.{IOException, InputStream, OutputStream}

import com.typesafe.scalalogging.Logger
import scalapb.{GeneratedMessage, GeneratedMessageCompanion, Message}

object Util {
  def timedTask[T](name: String, timeIt: Boolean = true)(thunk: => T)(
      implicit logger: Logger): T = {
    logger.debug(name)

    val time = System.currentTimeMillis()

    try {
      val result = thunk
      if (timeIt) {
        val elapsed = System.currentTimeMillis() - time
        logger.debug(name + " in " + elapsed)
      }
      result
    } catch {
      case e: Throwable =>
        logger.warn(name + " FAILED")
        throw e
    }
  }

  implicit class StreamingPBExtension[A <: GeneratedMessage with Message[A]](
      proto: GeneratedMessageCompanion[A]) {

    private def toBytes(x: Int): Array[Byte] =
      Array(
        (x >> 24).asInstanceOf[Byte],
        (x >> 16).asInstanceOf[Byte],
        (x >> 8).asInstanceOf[Byte],
        x.asInstanceOf[Byte]
      )

    private def fromBytes(x: Array[Byte]): Int =
      x(0) << 24 | (x(1) & 0xFF) << 16 | (x(2) & 0xFF) << 8 | (x(3) & 0xFF)

    private val messageTypeName = proto.scalaDescriptor.fullName

    def write(message: A, out: OutputStream): Int = {
      val bytes = message.toByteArray
      val size = bytes.length

      out.write(toBytes(size))
      out.write(bytes)

      size
    }

    def read(in: InputStream): Option[A] = {
      val sizeBuf = new Array[Byte](4)
      val sizeRead = in.read(sizeBuf)

      if (sizeRead == -1) {
        None
      } else if (sizeRead != 4) {
        throw new IOException(s"Unable to read message size, expected 4 bytes, read $sizeRead only")
      } else {
        val messageSize = fromBytes(sizeBuf)
        val messageBuf = new Array[Byte](messageSize)
        val messageRead = in.read(messageBuf, 0, messageSize)

        if (messageRead == -1) {
          throw new IOException(s"Unexpected EOF")
        } else if (messageRead != messageSize) {
          throw new IOException(
            s"Unable to read message $messageTypeName, expected $messageSize bytes, read $messageRead only")
        }

        Some(proto.parseFrom(messageBuf))
      }
    }

    def streamFrom(in: InputStream): Stream[A] = read(in) match {
      case Some(v) => v #:: streamFrom(in)
      case None => Stream.empty
    }
  }
}
