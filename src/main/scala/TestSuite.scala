package kantan.tests

import caps.unsafe.unsafeAssumePure
import collection.mutable.Builder

// ---------------------------------------------------------------------------------------------------------------------
//
// Groups tests together for ease of execution.
//
// This works by buffering all calls to an internal `Runner`, and passing them to an actual one later.
//
// ---------------------------------------------------------------------------------------------------------------------

/** Stores calls to `Runner.run` in order to run them later. */
private class TestWrapper(desc: String, test: (Rand, Params, Size, Assert) ?=> Unit, plan: Plan):
  val run: Runner ?->{test} Unit =
    Runner.run(desc, test, plan)

/** Set of tests, conventionally related to the same feature.
  *
  * The body of a `TestSuite` always has a `Runner` available, which allows implementers to write things like:
  * {{{
  *  class MyTest extends TestSuite:
  *    test("The foo should Bar"):
  *      assert(Rand.int(100) < 100, "Woops")
  * }}}
  */
trait TestSuite:
  private val testBuilder: Builder[TestWrapper, List[TestWrapper]] = List.newBuilder

  protected given Runner:
    override def run(desc: String, test: (Rand, Params, Size, Assert) ?=> Unit, plan: Plan) =
      testBuilder += TestWrapper(desc, test, plan).unsafeAssumePure

  val run: Runner ?-> Unit =
    testBuilder.result.foreach: test =>
      test.run
