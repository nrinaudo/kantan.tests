package kantan.tests

import caps.*

// ---------------------------------------------------------------------------------------------------------------------
//
// Functions used to run tests.
//
// ---------------------------------------------------------------------------------------------------------------------

// - Results of a test -------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
/** Status of a test, either a success or a failure. */
enum Result:
  case Success
  case Failure(shrinkCount: Int, msg: String, params: Params.Values)

/** Description of a test's execution. */
case class TestOutcome(successCount: Int, seed: Long, result: Result)

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
      Rand.Recorded(shrink(body, Result.Failure(0, msg, params), state, size), state)

// - Test running ------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
/** Runs the specified test, without attempting to shrink failing test cases.
  *
  * This is intended to replay known failing test cases, perhaps by using a known random seed or a recorded failing
  * state.
  */
def runOne(body: (Rand, Params, Size, Assert) ?=> Unit): (Rand, Size) ?->{body} Result =
  runTest(body) match
    case Params.Recorded(Assertion.Success, _)           => Result.Success
    case Params.Recorded(Assertion.Failure(msg), params) => Result.Failure(0, msg, params)

/** Runs the specified test, and attempts to shrink failing test cases.
  *
  * This is intended to replay known failing test cases, perhaps by using a known random seed or a recorded failing
  * state.
  */
def executeOne(body: (Rand, Params, Size, Assert) ?=> Unit): (Rand, Shrink, Size) ?->{body} Result =
  executeTest(body).value

case class Configuration(minSuccess: Int, minSize: Int, maxSize: Int)

/** Runs the specified test until the minimum number of successes has been reached.
  *
  * This will start by testing on smaller test cases (by using the minimum `size` value), and grow them until they reac
  * the maximum allowed size.
  *
  * Failed tests will be reduced in an attempt to provide a minimal reproduction scenario.
  */
def execute(conf: Configuration, body: (Rand, Params, Size, Assert) ?=> Unit): Shrink ?->{body} TestOutcome =
  val sizeStep = (conf.maxSize - conf.minSize) / conf.minSuccess

  def loop(count: Int, size: Int): TestOutcome =
    val seed = scala.util.Random.nextLong

    Size(size):
      Shrink:
        Rand.withSeed(seed):
          executeTest(size, body) match
            case Rand.Recorded(Result.Success, state) =>
              // If the state is empty, this is not a random-based test and there's no need to try it more than once.
              // If it *is* a random-based test, but we've ran it enough times, then the test is successful.
              val success = state.isEmpty || count >= conf.minSuccess

              if success then TestOutcome(count + 1, seed, Result.Success)
              else loop(count + 1, size + sizeStep)

            case Rand.Recorded(e: Result.Failure, _) => TestOutcome(count, seed, e)

  loop(0, conf.minSize)
