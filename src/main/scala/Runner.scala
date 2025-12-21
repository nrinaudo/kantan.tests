package kantan.tests

import caps.*

// ---------------------------------------------------------------------------------------------------------------------
//
// Test execution tools.
//
// ---------------------------------------------------------------------------------------------------------------------

case class Conf(minSuccess: Int, minSize: Int, maxSize: Int)

/** Capability describing the ability to run a test.
  *
  * A test here is really only a function that, given a `Conf`, yields some outcome.
  *
  * This is how we can provide a wealth of ways of running tests - for example, `testNoShrink` to skip failing test case
  * reduction.
  */
trait Runner extends SharedCapability:
  def run(name: String, body: Conf => TestOutcome): Unit

object Runner:
  /** Yields a "skipped" result with the specified message.
    *
    * This can be useful to implement an `ignore` test prompt, for example. Another typical use case would be for tests
    * that run in some enviroments but fail in others (have you tried Windows lately? yeah). One could relatively easily
    * write a prompt that checks the environment, calls `skip` for the bad ones, and runs the test for others.
    */
  def skip(name: String)(msg: String): Runner ?-> Unit =
    run(name): _ =>
      TestOutcome(0, Result.Skipped(msg))

  def run(name: String)(body: Conf => TestOutcome): Runner ?->{body} Unit = handler ?=> handler.run(name, body)
