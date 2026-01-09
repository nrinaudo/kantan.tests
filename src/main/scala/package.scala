package kantan.tests

// ---------------------------------------------------------------------------------------------------------------------
//
// Misc. utilities and types.
//
// I'm using this to stick all the things I don't really know where else to put.
//
// ---------------------------------------------------------------------------------------------------------------------

/** Runs a single test, without attempting to shrink failing test cases.
  *
  * This is intended for internal use only.
  */
private[tests] def runTest(
    test: (Rand, Params, Size, Assert) ?=> Unit
): (Rand, Size) ?->{test} Params.Recorded [AssertionResult] =
  Params:
    Assert:
      test

/** Configuration used when running a test.
  *
  * @param minSuccess
  *   the minimum number of successes that must be found for a test to be considered as passing.
  * @param minSize
  *   minimum value that a test's `Size` can start with.
  * @param maxSize
  *   maximum value that a test's `Size` can start with.
  */
case class Conf(minSuccess: Int, minSize: Int, maxSize: Int)

/** Describes a failing test case.
  *
  * @param msg
  *   message with which the test failed.
  * @param state
  *   how to replay the failing test case, using for example `Prompt.replay`.
  */
case class FailingTestCase(msg: String, state: ReplayState)
