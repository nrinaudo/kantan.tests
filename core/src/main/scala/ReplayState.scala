package kantan.tests

import java.util.zip.*
import java.io.*
import java.util.Base64
import scala.util.Try

case class ReplayState(randState: Rand.State, size: Int)

object ReplayState:
  private def readState(in: DataInputStream) =
    val size = in.readInt
    val out  = List.newBuilder[Int]

    (0 until size).foreach: _ =>
      out += in.readInt

    out.result

  private def writeState(randState: List[Int], out: DataOutputStream) =
    out.writeInt(randState.length)
    randState.foreach(out.writeInt)

  def encode(state: ReplayState): String =
    val bytes = ByteArrayOutputStream()
    val out   = DataOutputStream(GZIPOutputStream(bytes, true))

    out.writeInt(state.size)
    writeState(state.randState.toInts, out)
    out.close()

    Base64.getEncoder().encodeToString(bytes.toByteArray)

  def decode(savedState: String): Option[ReplayState] =
    val result = Try:
      val bytes     = Base64.getDecoder().decode(savedState)
      val in        = DataInputStream(GZIPInputStream(ByteArrayInputStream(bytes)))
      val size      = in.readInt
      val randState = readState(in)

      ReplayState(Rand.State(randState), size)

    result.toOption
