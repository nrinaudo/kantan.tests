package kantan.tests

import caps.*

// ---------------------------------------------------------------------------------------------------------------------
//
// Test execution tools.
//
// ---------------------------------------------------------------------------------------------------------------------

/** Capability describing the ability to run a test.
  *
  * A test here is really only a function that, given a `Conf`, yields some outcome.
  *
  * This is how we can provide a wealth of ways of running tests - for example, `testNoShrink` to skip failing test case
  * reduction.
  */
trait Runner extends SharedCapability:
  def run(name: String, body: Conf => Runner.Outcome): Unit

object Runner:
  // - Test results ----------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  case class Outcome(successCount: Int, result: Result)

  enum Result:
    case Success
    case Failure(msg: String, shrinkCount: Int, replay: ReplayState, params: Params.Values)
    case Skipped(msg: String)

    def isSuccess = this match
      case Success => true
      case _       => false

    def isFailure = !isSuccess

  // - Runner DSL ------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** Yields a "skipped" result with the specified message.
    *
    * This can be useful to implement an `ignore` test prompt, for example. Another typical use case would be for tests
    * that run in some enviroments but fail in others (have you tried Windows lately? yeah). One could relatively easily
    * write a prompt that checks the environment, calls `skip` for the bad ones, and runs the test for others.
    */
  def skip(name: String)(msg: String): Runner ?-> Unit =
    run(name): _ =>
      Outcome(0, Result.Skipped(msg))

  def run(name: String)(body: Conf => Outcome): Runner ?->{body} Unit =
    handler ?=> handler.run(name, body)
