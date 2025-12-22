package kantan.tests

import caps.*
import Prompt.*

// ---------------------------------------------------------------------------------------------------------------------
//
// Test plans.
//
// A test plan describes how to go about attempting to find failing test cases for a given test. Of course, it's kind
// of useless for non-generative tests, but the point of kantan.tests is to slowly edge people towards them, so...
//
// The default test plan, `growing`, will start with the smallest test cases possible, and slowly grow to the largest
// ones allowed by the configuration.
//
// Note that this is entirely unrelated to test case reduction - "shrinking". This happens *after* a failing test case
// has been found.
//
// ---------------------------------------------------------------------------------------------------------------------

// - Test configuration ------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------

case class Conf(minSuccess: Int, minSize: Int, maxSize: Int)

// - Test plan ---------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
/** Describes the ability to search for a failing test case for a given test. */
trait Plan extends SharedCapability:
  def execute(conf: Conf, test: (Rand, Params, Size, Assert) ?=> Unit): Plan.Outcome

object Plan:

  // - Plan results ----------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** Describes the outcome of a test plan. */
  case class Outcome(successCount: Int, result: Result)

  /** Describes the result of a test plan. */
  enum Result:
    case Success
    case Failure(msg: String, state: ReplayState, params: Params.Values)

  // - Plan execution --------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def execute(conf: Conf, test: (Rand, Params, Size, Assert) ?=> Unit): Plan ?->{test} Outcome =
    handler ?=> handler.execute(conf, test)

  /** Provides a "growing" test plan.
    *
    * This plan will start from the smallest allowed `size`, and grow it little by little until it reaches the maximum
    * allowed one.
    */
  def growing[A](body: Plan ?=> A): A =
    given Plan:
      // Provides all handlers for running a single test.
      def run(size: Int, test: (Rand, Params, Size, Assert) ?=> Unit) =
        Size(size):
          Rand:
            Rand.record:
              runTest(test)

      def execute(conf: Conf, test: (Rand, Params, Size, Assert) ?=> Unit): Outcome =
        val sizeStep = (conf.maxSize - conf.minSize) / conf.minSuccess

        def loop(count: Int, size: Int): Outcome =
          run(size, test) match
            case Rand.Recorded(Params.Recorded(Assertion.Success, _), state) =>
              // If the state is empty, this is not a random-based test and there's no need to try it more than once.
              // If it *is* a random-based test, but we've ran it enough times, then the test is successful.
              val success = state.isEmpty || count >= conf.minSuccess - 1

              if success then Outcome(count + 1, Result.Success)
              else loop(count + 1, size + sizeStep)

            case Rand.Recorded(Params.Recorded(Assertion.Failure(msg), params), state) =>
              Outcome(count, Result.Failure(msg, ReplayState(state, size), params))

        loop(0, conf.minSize)

    body
