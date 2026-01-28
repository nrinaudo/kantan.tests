package kantan.tests

import caps.*
import scala.util.control.*
import scala.caps.unsafe.unsafeAssumePure

// ---------------------------------------------------------------------------------------------------------------------
//
// Simple assertions.
//
// This allows tests to call `fail`, or any of its variants, to fail out of a test case cleanly.
//
// ---------------------------------------------------------------------------------------------------------------------

/** Internal exception used to "jump" back to the specified `Assert` */
private case class AssertionFailure(label: Assert^{}, message: String) extends ControlThrowable with NoStackTrace

/** Result of an assertion block. */
enum AssertionResult:
  case Success
  case Failure(msg: String)

/** Capability to assert something and abort the computation should the assertion fail. */
sealed abstract class Assert extends SharedCapability

object Assert:
  /** Runs the specified body, turning errors into `Assertion.Failure`s. */
  def apply(body: Assert ?=> Unit): AssertionResult =
    given label: Assert = new Assert {}

    try
      body
      AssertionResult.Success
    catch
      case AssertionFailure(`label`, message) => AssertionResult.Failure(message)
      case e: AssertionFailure                => throw e
      case e                                  => AssertionResult.Failure(e.getMessage)

  /** Fails with the specified message, jumping back to label denoted by whatever `Assert` handler is used. */
  def fail(msg: String)(using label: Assert): Nothing =
    throw AssertionFailure(label.unsafeAssumePure, msg)

  def assert(b: Boolean)(using Assert): Unit =
    assert(b, "Assertion did not hold")

  /** Fails with the specified message if `b` is `false`. */
  def assert(b: Boolean, msg: String)(using Assert): Unit =
    if b then ()
    else fail(msg)

  def assertEquals[A](lhs: A, rhs: A)(using Assert): Unit =
    assert(lhs == rhs, s"$lhs was not equal to $rhs")
