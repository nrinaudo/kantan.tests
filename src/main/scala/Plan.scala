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
  def execute(test: (Rand, Params, Size, Assert) ?=> Unit, conf: Conf): Run.Outcome

  def mapConf(f: Conf => Conf): Plan^{this, f} =
    (test: (Rand, Params, Size, Assert) ?=> Unit, conf: Conf) => execute(test, f(conf))

object Plan:
  /** Typical search then shrink test execution. */
  def execute(test: (Rand, Params, Size, Assert) ?=> Unit, conf: Conf)(using Shrink, Search): Run.Outcome =

    def shrink(originalTestCase: FailingTestCase, originalParams: Params.Values): Run.Result =
      val Shrink.Result(testCase, shrinkCount, params) =
        Shrink
          .shrink(test, originalTestCase)
          .getOrElse(Shrink.Result(originalTestCase, 0, originalParams))

      Run.Result.Failure(testCase.msg, shrinkCount, testCase.state, params)

    val Search.Result(testCase, successCount, params) = Search.search(conf, test)
    Run.Outcome(
      successCount,
      testCase
        .map(shrink(_, params))
        .getOrElse(Run.Result.Success(params))
    )

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
    (_: (Rand, Params, Size, Assert) ?=> Unit, _: Conf) => Run.Outcome(0, Run.Result.Skipped(msg))

  def replay(state: ReplayState): Plan =
    (test: (Rand, Params, Size, Assert) ?=> Unit, conf: Conf) =>
      Size(state.size):
        Rand.replay(state.randState):
          runTest(test) match
            case Params.Recorded(AssertionResult.Success, params) =>
              Run.Outcome(1, Run.Result.Success(params))

            case Params.Recorded(AssertionResult.Failure(msg), params) =>
              Run.Outcome(0, Run.Result.Failure(msg, 0, state, params))
