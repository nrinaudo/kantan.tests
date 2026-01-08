package kantan.tests

// ---------------------------------------------------------------------------------------------------------------------
//
// Test prompts.
//
// A test prompt is a way of declaring a test, which usually looks something like:
// test("Test the foobar"):
//   assertEquals("foo", "bar")
//
// ---------------------------------------------------------------------------------------------------------------------

object Prompt:
  def test(desc: String)(body: (Rand, Log, Size, Assert) ?=> Unit): Run ?->{body} Unit =
    grow(desc)(body)

  object grow:
    def apply(desc: String)(body: (Rand, Log, Size, Assert) ?=> Unit): Run ?->{body} Unit =
      Run.run(desc, body, Plan.grow)

    def noShrink(desc: String)(body: (Rand, Log, Size, Assert) ?=> Unit): Run ?->{body} Unit =
      Run.run(desc, body, Plan.growNoShrink)

  def enumerate(desc: String)(body: (Rand, Log, Size, Assert) ?=> Unit): Run ?->{body} Unit =
    Run.run(desc, body, Plan.enumerate)

  def ignore(desc: String)(body: (Rand, Log, Size, Assert) ?=> Unit): Run ?->{body} Unit =
    Run.run(desc, body, Plan.ignore("Test marked as ignored"))

    /** Runs the specified test exactly once, ignoring configuration and using the specified parameters instead.
      *
      * This is intended to easily replay failing test cases.
      */
  def replay(desc: String)(state: ReplayState)(test: (Rand, Log, Size, Assert) ?=> Unit): Run ?->{test} Unit =
    Run.run(desc, test, Plan.replay(state))

  /** Runs the specified test exactly once, ignoring configuration and using the state denoted by the specified string.
    *
    * The `state` parameter can be obtained by observing a failing test report, it will include something like:
    * {{{
    *  Replay: H4sIAAAAAAAA_2JgAANGEAEAAAD__w==
    * }}}
    */
  def replay(desc: String)(state: String)(test: (Rand, Log, Size, Assert) ?=> Unit): Run ?->{test} Unit =
    ReplayState.decode(state) match
      case None        => Run.run(desc, test, Plan.ignore("Failed to decode replay state"))
      case Some(state) => replay(desc)(state)(test)
