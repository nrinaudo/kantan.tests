package kantan.tests

import caps.*
import collection.mutable.Map as MutableMap

// ---------------------------------------------------------------------------------------------------------------------
//
// Used to record parameter values.
//
// This allows failing tests to report what they failed on.
//
// ---------------------------------------------------------------------------------------------------------------------

/** Capability allowing tests to keep track of important parameters.
  *
  * Calling `param` associates the specified parameter `name` to the specified `value` - this is typically useful to
  * display failing test cases.
  */
trait Params extends SharedCapability:
  def param[A](name: String, value: A): Unit

object Params:
  case class Values(values: Map[String, String])
  object Values:
    val empty: Values = Values(Map.empty)

  case class Recorded[A](value: A, params: Values)

  def apply[A](body: Params ?=> A): Recorded[A] =
    val records = MutableMap.empty[String, String]

    given Params:
      override def param[A](name: String, value: A) =
        records += name -> value.toString

    Recorded(body, Values(records.toMap))

  def param[A](name: String, value: A): Params ?-> A = r ?=>
    r.param(name, value)
    value
