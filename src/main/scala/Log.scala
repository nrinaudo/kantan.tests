package kantan.tests

// ---------------------------------------------------------------------------------------------------------------------
//
// Logging during tests.
//
// ---------------------------------------------------------------------------------------------------------------------

import caps.*
import collection.mutable.Map as MutableMap

/** Exposes ways of logging useful information during tests.
  *
  * These are different from `println` statements in that they're passed downstream, potentially to make their way to
  * test reports, for example.
  *
  * A typical use case is to expose the important inputs of a test, which is ever so helpful when trying to understand
  * failures. You can do so with the `logAs` extension method:
  *
  * {{{
  * val name = Rand.identifier.logAs("Name")
  * }}}
  */
trait Log extends SharedCapability:
  /** Logs the specified message. */
  def log(msg: String): Unit

  /** Logs an input to a test, with the specified name and value. */
  def logInput(name: String, value: String): Unit

  extension [A](a: A)
    def logAs(name: String): A =
      logInput(name, a.toString)
      a

object Log:

  // - Log DSL ---------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def log(msg: String): Log ?-> Unit                    = handler ?=> handler.log(msg)
  def logInput[A](name: String, value: A): Log ?-> Unit = value.logAs(name)

  // - Recorded logs ---------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** Stores all logged inputs for a test run. */
  opaque type Inputs = Map[String, String]
  object Inputs:
    inline def apply(inputs: Map[String, String]): Inputs            = inputs
    extension (inputs: Inputs) inline def toMap: Map[String, String] = inputs

  /** Stores all free-form logs for a test run. */
  opaque type Entries = List[String]
  object Entries:
    inline def apply(entries: List[String]): Entries             = entries
    extension (entries: Entries) inline def toList: List[String] = entries

  /** Aggregates the result of an effectful computation and its logs. */
  case class Recorded[A](value: A, inputs: Inputs, logs: Log.Entries)

  // - Handler(s) ------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** Runs the specified effectful computation, storing logs in a [[Recorded]]. */
  def apply[A](body: Log ?=> A): Recorded[A] =
    val inputs = MutableMap.empty[String, String]
    val logs   = List.newBuilder[String]

    given Log = new Log:
      override def log(msg: String)                      = logs += msg
      override def logInput(name: String, value: String) = inputs(name) = value

    Recorded(body, inputs.toMap, logs.result)
