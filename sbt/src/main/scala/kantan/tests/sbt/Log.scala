package kantan.tests.sbt

import caps.*
import _root_.sbt.testing.Logger
import scala.io.AnsiColor

// ---------------------------------------------------------------------------------------------------------------------
//
// Wrapper for SBT's `Logger`.
//
// ---------------------------------------------------------------------------------------------------------------------

/** Represents the ability to send write logs via SBT's framework.
  *
  * This uses a very small subset of what SBT offers, but it's really all we need: writing a string to stdout, with
  * colors when supported.
  */
trait Log extends SharedCapability:
  def log(msg: String, color: Color): Unit

object Log:
  def apply[A](loggers: Array[Logger])(body: Log ?=> A): A =
    given Log = (msg, color) =>
      loggers.foreach: logger =>
        if logger.ansiCodesSupported then logger.info(s"$color$msg${Color.Reset}")
        else logger.info(msg)

    body

  def log(msg: String, color: Color)(using handler: Log): Unit =
    handler.log(msg, color)

enum Color:
  override def toString =
    this match
      case Magenta => AnsiColor.MAGENTA
      case Blue    => AnsiColor.BLUE
      case Red     => AnsiColor.RED
      case Cyan    => AnsiColor.CYAN
      case Reset   => AnsiColor.RESET
      case Green   => AnsiColor.GREEN
      case Yellow  => AnsiColor.YELLOW
      case White   => AnsiColor.WHITE
      case Black   => AnsiColor.BLACK

  case Magenta
  case Blue
  case Red
  case Cyan
  case Reset
  case Green
  case Yellow
  case White
  case Black
