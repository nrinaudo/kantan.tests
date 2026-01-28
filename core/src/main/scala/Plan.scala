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
  def execute(test: (Assert, Log, Rand, Size) ?=> Unit, conf: Conf): TestResult

  def mapConf(f: Conf => Conf): Plan^{this, f} =
    (test: (Assert, Log, Rand, Size) ?=> Unit, conf: Conf) => execute(test, f(conf))

object Plan:
  // - Standard plan ----------------------------------------------------------------------------------------------------
  // --------------------------------------------------------------------------------------------------------------------
  /** Typical search then shrink test execution. */
  def execute(test: (Assert, Log, Rand, Size) ?=> Unit, conf: Conf)(using Shrink, Search): TestResult =

    // Shrinks a known failing test case and returns the resulting test status.
    def shrink(
        originalTestCase: FailingTestCase,
        successCount: Int
    ): TestResult =
      val Shrink.Result(testCase, shrinkCount) =
        Shrink
          .shrink(test, originalTestCase)
          .getOrElse(Shrink.Result(originalTestCase, 0))

      TestResult.Failure(testCase, shrinkCount, successCount)

    // Searches for a failing test case, then attempts to shrink it.
    val Search.Result(testCase, successCount) = Search.search(conf, test)
    testCase
      .map(shrink(_, successCount))
      .getOrElse(TestResult.Success(successCount))

  // - Grow plans -------------------------------------------------------------------------------------------------------
  // --------------------------------------------------------------------------------------------------------------------
  val grow: Plan =
    (test: (Assert, Log, Rand, Size) ?=> Unit, conf: Conf) =>
      Search.grow:
        Shrink.Naive:
          Shrink.Naive.caching(1000):
            execute(test, conf)

  val growNoShrink: Plan =
    (test: (Assert, Log, Rand, Size) ?=> Unit, conf: Conf) =>
      Search.grow:
        Shrink.noop:
          execute(test, conf)

  // - Enumeration plans ------------------------------------------------------------------------------------------------
  // --------------------------------------------------------------------------------------------------------------------
  val enumerate: Plan =
    (test: (Assert, Log, Rand, Size) ?=> Unit, conf: Conf) =>
      Shrink.noop:
        Search.enumerate:
          execute(test, conf)

  // - "Administrative" plans -------------------------------------------------------------------------------------------
  // --------------------------------------------------------------------------------------------------------------------
  def ignore(msg: String): Plan =
    (_: (Assert, Log, Rand, Size) ?=> Unit, _: Conf) => TestResult.Skipped(msg)

  def replay(state: ReplayState): Plan =
    (test: (Assert, Log, Rand, Size) ?=> Unit, conf: Conf) =>
      Size(state.size):
        Rand.replay(state.randState):
          runTest(test) match
            case Log.Recorded(AssertionResult.Success, _, _) =>
              TestResult.Success(0)

            case Log.Recorded(AssertionResult.Failure(msg), inputs, logs) =>
              val testCase = FailingTestCase(msg, state, inputs, logs)
              TestResult.Failure(testCase, 0, 0)
