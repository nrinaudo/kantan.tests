package kantan.tests

// ---------------------------------------------------------------------------------------------------------------------
//
// Test plans.
//
// ---------------------------------------------------------------------------------------------------------------------

/** A specific way of running a test.
  *
  * A typical plan involves using a `Search` to find a failing test case, followed by a `Shrink` to try and reduce it.
  * You're encouraged to experiment with weirder plans though, that is the reason for the existence of `Plan`.
  */
trait Plan:
  def execute(test: (Rand, Params, Size, Assert) ?=> Unit, conf: Conf): TestResult

  def mapConf(f: Conf => Conf): Plan^{this, f} =
    (test: (Rand, Params, Size, Assert) ?=> Unit, conf: Conf) => execute(test, f(conf))

object Plan:
  /** Typical search then shrink test execution. */
  def execute(test: (Rand, Params, Size, Assert) ?=> Unit, conf: Conf)(using Shrink, Search): TestResult =

    // Shrinks a known failing test case and returns the resulting test status.
    def shrink(originalTestCase: FailingTestCase, originalParams: Params.Values): TestResult.Status =
      val Shrink.Result(testCase, shrinkCount, params) =
        Shrink
          .shrink(test, originalTestCase)
          .getOrElse(Shrink.Result(originalTestCase, 0, originalParams))

      TestResult.Status.Failure(testCase.msg, shrinkCount, testCase.state)

    // Searches for a failing test case, then attempts to shrink it.
    val Search.Result(testCase, successCount, params) = Search.search(conf, test)
    val status                                        = testCase
      .map(shrink(_, params))
      .getOrElse(TestResult.Status.Success)

    TestResult(successCount, params, status)

  val grow: Plan =
    (test: (Rand, Params, Size, Assert) ?=> Unit, conf: Conf) =>
      Search.grow:
        Shrink.Naive:
          Shrink.Naive.caching(1000):
            execute(test, conf)

  val growNoShrink: Plan =
    (test: (Rand, Params, Size, Assert) ?=> Unit, conf: Conf) =>
      Search.grow:
        Shrink.noop:
          execute(test, conf)

  val enumerate: Plan =
    (test: (Rand, Params, Size, Assert) ?=> Unit, conf: Conf) =>
      Shrink.noop:
        Search.enumerate:
          execute(test, conf)

  def ignore(msg: String): Plan =
    (_: (Rand, Params, Size, Assert) ?=> Unit, _: Conf) => TestResult.skipped(msg)

  def replay(state: ReplayState): Plan =
    (test: (Rand, Params, Size, Assert) ?=> Unit, conf: Conf) =>
      Size(state.size):
        Rand.replay(state.randState):
          runTest(test) match
            case Params.Recorded(AssertionResult.Success, params) =>
              TestResult.success(0, params)

            case Params.Recorded(AssertionResult.Failure(msg), params) =>
              TestResult.failure(0, params, msg, 0, state)
