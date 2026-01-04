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
    val outcome = Search.search(conf, test)

    val result = outcome.failure match
      case Some(failure) =>
        val shrunk = Shrink.shrink(test, failure)
        Run.Result.Failure(shrunk.failure.msg, shrunk.shrinkCount, shrunk.failure.state, shrunk.failure.params)

      case None => Run.Result.Success

    Run.Outcome(outcome.successCount, result)

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
        Rand.replay(state.state):
          runTest(test) match
            case Params.Recorded(Assertion.Success, _) =>
              Run.Outcome(1, Run.Result.Success)

            case Params.Recorded(Assertion.Failure(msg), params) =>
              Run.Outcome(0, Run.Result.Failure(msg, 0, state, params))
