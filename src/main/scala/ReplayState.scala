package kantan.tests

import java.util.zip.*
import java.io.*
import java.util.Base64
import scala.util.Try

case class ReplayState(randState: Rand.State, size: Int):
  private def writeState(randState: List[Int], out: DataOutputStream) =
    out.writeInt(randState.length)
    randState.foreach(out.writeInt)

  def encode: String =
    val bytes = ByteArrayOutputStream()
    val out   = DataOutputStream(GZIPOutputStream(bytes, true))

    out.writeInt(size)
    writeState(randState.toInts, out)
    out.flush()

    Base64.getUrlEncoder().encodeToString(bytes.toByteArray)

object ReplayState:
  private def readState(in: DataInputStream) =
    val size = in.readInt
    val out  = List.newBuilder[Int]

    (0 until size).foreach: _ =>
      out += in.readInt

    out.result

  def decode(savedState: String): Option[ReplayState] =
    val result = Try:
      val bytes     = Base64.getUrlDecoder().decode(savedState)
      val in        = DataInputStream(GZIPInputStream(ByteArrayInputStream(bytes)))
      val size      = in.readInt
      val randState = readState(in)

      ReplayState(Rand.State(randState), size)

    result.toOption
