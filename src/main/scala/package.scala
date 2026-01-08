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
    test: (Rand, Log, Size, Assert) ?=> Unit
): (Rand, Size) ?->{test} Log.Recorded [AssertionResult] =
  Log:
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
  * @param inputs
  *   name / value mapping for the inputs that caused this failure.
  * @param logs
  *   all logs generated during the test.
  */
case class FailingTestCase(msg: String, state: ReplayState, inputs: Log.Inputs, logs: Log.Entries)

/** Describes the result of running a single test. */
enum TestResult:
  /** The test was skipped.
    * @param msg
    *   explains why the test was skipped.
    */
  case Skipped(msg: String)

  /** The test never failed.
    * @param successCount
    *   number of times the test was evaluated successfully.
    */
  case Success(successCount: Int)

  /** The test failed.
    * @param testCase
    *   describes how the test was failed and how to reproduce it.
    * @param shrinkCount
    *   number of times the failing test case was reduced from its original size.
    * @param successCount
    *   number of times the test was evaluated successfully.
    */
  case Failure(
      testCase: FailingTestCase,
      shrinkCount: Int,
      successCount: Int
  )
