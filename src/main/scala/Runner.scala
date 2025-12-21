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
   * End-users should probably call `ignore` instead.
   */
  def skip(name: String)(msg: String): Runner ?-> Unit =
    run(name): _ =>
      TestOutcome(0, Result.Skipped(msg))

  def run(name: String)(body: Conf => TestOutcome): Runner ?->{body} Unit = handler ?=>
    handler.run(name, body)

  /** Runs the specified test without shrinking failing test cases. */
  def testNoShrink(desc: String)(body: (Rand, Params, Size, Assert) ?=> Unit): Runner ?->{body} Unit =
    run(desc): conf =>
      Plan.growing:
        Plan.execute(conf, body)

  /** Runs the specified test, shrinking failing test cases if found. */
  def test(desc: String)(body: (Rand, Params, Size, Assert) ?=> Unit): Runner ?->{body} Unit =
    run(desc): conf =>
      Plan.growing:
        Plan.execute(conf, body) match
          case TestOutcome(count, failure: Result.Failure) =>
            Shrink:
              Shrink.caching(1000):
                TestOutcome(count, shrink(body, failure))

          case other => other

  /** Runs the specified exactly once, ignoring configuration and using the specified parameters instead.
    *
    * This is intended to easily replay failing test cases.
    */
  def replay(desc: String)(state: ReplayState)(body: (Rand, Params, Size, Assert) ?=> Unit): Runner ?->{body} Unit =
    run(desc): _ =>
      Size(state.size):
        Rand.replay(state.state):
          runTest(body) match
            case Params.Recorded(Assertion.Success, _)           =>
              TestOutcome(1, Result.Success)

            case Params.Recorded(Assertion.Failure(msg), params) =>
              TestOutcome(0, Result.Failure(0, msg, state, params))

  /** Runs the specified exactly once, ignoring configuration and using the state denoted by the specified string.
    *
    * Valid states are given as the output of failing test cases.
    */
  def replay(desc: String)(state: String)(body: (Rand, Params, Size, Assert) ?=> Unit): Runner ?->{body} Unit =
    ReplayState.decode(state) match
      case None        => skip(desc)("Failed to decode replay state")
      case Some(state) => replay(desc)(state)(body)

  def ignore(desc: String)(body: (Rand, Params, Size, Assert) ?=> Unit): Runner ?->{body} Unit =
    skip(desc)("Test marked as ignored")

// - Internal test running ---------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------

/** Runs a single test, without attempting to shrink failing test cases.
  *
  * This is intended for internal use only.
  */
private def runTest(body: (Rand, Params, Size, Assert) ?=> Unit): (Rand, Size) ?->{body} Params.Recorded [Assertion] =
  Params:
    Assert:
      body
