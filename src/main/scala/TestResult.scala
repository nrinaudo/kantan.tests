package kantan.tests

// ---------------------------------------------------------------------------------------------------------------------
//
// Results of running a single test.
//
// ---------------------------------------------------------------------------------------------------------------------

/** Describes the result of running a single test. */
enum TestResult:
  case Skipped(msg: String)
  case Ran(successCount: Int, params: Params.Values, status: TestResult.Status)

object TestResult:
  enum Status:
    case Failure(msg: String, shrinkCount: Int, replay: ReplayState)
    case Success

  def apply(successCount: Int, params: Params.Values, status: TestResult.Status): TestResult =
    TestResult.Ran(successCount, params, status)

  def skipped(msg: String): TestResult =
    TestResult.Skipped(msg)

  def success(successCount: Int, params: Params.Values): TestResult =
    TestResult.Ran(successCount, params, Status.Success)

  def failure(
      successCount: Int,
      params: Params.Values,
      msg: String,
      shrinkCount: Int,
      replay: ReplayState
  ): TestResult =
    TestResult.Ran(successCount, params, Status.Failure(msg, shrinkCount, replay))
