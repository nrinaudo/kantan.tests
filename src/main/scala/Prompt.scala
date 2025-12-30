package kantan.tests

// ---------------------------------------------------------------------------------------------------------------------
//
// Test prompts.
//
// A test prompt is a way of declaring a test, which usually looks something like:
// test("Test the foobar"):
//   assertEquals("foo", "bar")
//
// Default ones are found in the cleverly named `default` namespace, but brave users can write their own - by, for
// example, using the ones found in `core` as building blocks.
//
// ---------------------------------------------------------------------------------------------------------------------

object Prompt:
  /** Runs a single test, without attempting to shrink failing test cases.
    *
    * This is intended for internal use only.
    */
  private[tests] def runTest(
      test: (Rand, Params, Size, Assert) ?=> Unit
  ): (Rand, Size) ?->{test} Params.Recorded [Assertion] =
    Params:
      Assert:
        test

  /** Core prompts, allowing callers to provide their own handlers.
    *
    * These really aren't meant to be called by end-users, but rather used as basic building blocks for more directly
    * usable prompts.
    */
  object core:

    /** Runs the specified test, shrinking failing test cases if needed. */
    def test(desc: String)(test: (Rand, Params, Size, Assert) ?=> Unit): (Search, Runner, Shrink) ?->{test} Unit =
      Runner.run(desc): conf =>
        val outcome = Search.search(conf, test)

        val result = outcome.failure match
          case Some(failure) =>
            val shrunk = Shrink.shrink(test, failure)
            Runner.Result.Failure(shrunk.failure.msg, shrunk.shrinkCount, shrunk.failure.state, shrunk.failure.params)

          case None => Runner.Result.Success

        Runner.Outcome(outcome.successCount, result)

    /** Runs the specified test, without shrinking failing test cases. */
    def testNoShrink(desc: String)(test: (Rand, Params, Size, Assert) ?=> Unit): (Search, Runner) ?->{test} Unit =
      Shrink.noop:
        core.test(desc)(test)

  object enumerate:
    /** Runs the specified test by attempting to enumerate all small test cases. */
    def test(desc: String)(test: (Rand, Params, Size, Assert) ?=> Unit): Runner ?->{test} Unit =
      Search.enumerate:
        core.testNoShrink(desc)(test)

  /** Provides default test prompts, the one users should be using unless they have a pretty good idea what they're
    * doing.
    */
  object default:
    /** Runs the specified test, shrinking failing test cases if found. */
    def test(desc: String)(test: (Rand, Params, Size, Assert) ?=> Unit): Runner ?->{test} Unit =
      Search.linearGrowth:
        Shrink.Naive:
          Shrink.Naive.caching(1000):
            core.test(desc)(test)

    /** Runs the specified test without shrinking failing test cases.
      *
      * This might be desirable for tests that are particularly expensive to run on large test cases, for example.
      */
    def testNoShrink(desc: String)(test: (Rand, Params, Size, Assert) ?=> Unit): Runner ?->{test} Unit =
      Search.linearGrowth:
        core.testNoShrink(desc)(test)

    /** Runs the specified test exactly once, ignoring configuration and using the specified parameters instead.
      *
      * This is intended to easily replay failing test cases.
      */
    def replay(desc: String)(state: ReplayState)(test: (Rand, Params, Size, Assert) ?=> Unit): Runner ?->{test} Unit =
      Runner.run(desc): _ =>
        Size(state.size):
          Rand.replay(state.state):
            runTest(test) match
              case Params.Recorded(Assertion.Success, _) =>
                Runner.Outcome(1, Runner.Result.Success)

              case Params.Recorded(Assertion.Failure(msg), params) =>
                Runner.Outcome(0, Runner.Result.Failure(msg, 0, state, params))

    /** Runs the specified test exactly once, ignoring configuration and using the state denoted by the specified
      * string.
      *
      * The `state` parameter can be obtained by observing a failing test report, it will include something like:
      * {{{
      *  Replay: H4sIAAAAAAAA_2JgAANGEAEAAAD__w==
      * }}}
      */
    def replay(desc: String)(state: String)(test: (Rand, Params, Size, Assert) ?=> Unit): Runner ?->{test} Unit =
      ReplayState.decode(state) match
        case None        => Runner.skip(desc)("Failed to decode replay state")
        case Some(state) => replay(desc)(state)(test)

    /** Ignores the specified test.
      *
      * This is useful for tests that are currently broken, but which you don't want to delete in the unlikely hope that
      * someone will eventually get around to fixing it. Oh, sweet summer child...
      */
    def ignore(desc: String)(test: (Rand, Params, Size, Assert) ?=> Unit): Runner ?-> Unit =
      Runner.skip(desc)("Test marked as ignored")
