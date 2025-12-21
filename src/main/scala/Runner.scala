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
      Shrink.noop:
        execute(conf, body)

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

  def test(desc: String)(body: (Rand, Params, Size, Assert) ?=> Unit): Runner ?->{body} Unit =
    run(desc): conf =>
      Shrink:
        Shrink.caching(1000):
          execute(conf, body)

  def ignore(desc: String)(body: (Rand, Params, Size, Assert) ?=> Unit): Runner ?->{body} Unit =
    skip(desc)("Test marked as ignored")

// - Results of a test -------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
/** Status of a test, either a success or a failure. */
enum Result:
  case Success
  case Failure(shrinkCount: Int, msg: String, replay: ReplayState, params: Params.Values)
  case Skipped(msg: String)

  def isSuccess = this match
    case Success    => true
    case _ => false

  def isFailure = !isSuccess

/** Description of a test's execution. */
case class TestOutcome(successCount: Int, result: Result)

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

/** Runs a single test, and attempts to shrink failing test cases.
  *
  * This is intended for internal purposes only.
  */
private def executeTest(
    body: (Rand, Params, Size, Assert) ?=> Unit
): (Rand, Shrink, Size) ?->{body} Rand.Recorded [Result] =
  val size = Size.size

  Rand.record(runTest(body)) match
    case Rand.Recorded(Params.Recorded(Assertion.Success, _), state) =>
      Rand.Recorded(Result.Success, state)

    case Rand.Recorded(Params.Recorded(Assertion.Failure(msg), params), state) =>
      Rand.Recorded(shrink(body, Result.Failure(0, msg, ReplayState(state, size), params)), state)

// - Test running ------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
/** Runs the specified test until the minimum number of successes has been reached.
  *
  * This will start by testing on smaller test cases (by using the minimum `size` value), and grow them until they reac
  * the maximum allowed size.
  *
  * Failed tests will be reduced in an attempt to provide a minimal reproduction scenario.
  */
def execute(conf: Conf, body: (Rand, Params, Size, Assert) ?=> Unit): Shrink ?->{body} TestOutcome =
  val sizeStep = (conf.maxSize - conf.minSize) / conf.minSuccess

  def loop(count: Int, size: Int): TestOutcome =
    val seed = scala.util.Random.nextLong

    Size(size):
      Rand.withSeed(seed):
        executeTest(size, body) match
          case Rand.Recorded(Result.Success, state) =>
            // If the state is empty, this is not a random-based test and there's no need to try it more than once.
            // If it *is* a random-based test, but we've ran it enough times, then the test is successful.
            val success = state.isEmpty || count >= conf.minSuccess - 1

            if success then TestOutcome(count + 1, Result.Success)
            else loop(count + 1, size + sizeStep)

          case Rand.Recorded(other, _) => TestOutcome(count, other)

  loop(0, conf.minSize)
