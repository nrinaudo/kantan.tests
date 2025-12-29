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

  // - Default plan ----------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
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

      override def execute(conf: Conf, test: (Rand, Params, Size, Assert) ?=> Unit) =
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

  // - "Exhaustive" plan -----------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** Provides a plan that will attempt to exhaustively enumerate "small" test cases.
    *
    * This is heavily inspired by `SmallCheck and Lazy SmallCheck`, which reasons that most tests that fail, do so on
    * some very small test cases.
    *
    * This is probably not the right default plan, but is probably a good one to use in a more interactive mode -
    * playing maybe by starting a test with this plan to find all the low hanging fruits, and then move to one that
    * casts a wider net.
    */
  def exhaust[A](body: Plan ?=> A): A =
    given Plan:

      // Product of the minimum value in a state and the index at which it was found. Because I hate tuples.
      case class Min(index: Int, value: Int)

      // Finds the smallest value in the specified list of ints (right-leaning).
      def minWithIndex(is: List[Int]): Option[Min] =
        def go(i: Int, min: Min, is: List[Int]): Min =
          is match
            case head :: tail if head <= min.value => go(i + 1, Min(i, head), tail)
            case head :: tail                      => go(i + 1, min, tail)
            case _                                 => min

        is match
          case head :: tail => Some(go(1, Min(0, head), tail))
          case _            => None

      // Computes the next state, which should be the smallest one bigger than `state`.
      def nextState(state: Rand.State): Option[Rand.State] =
        val values = state.toInts

        // We may not get a minimum value in the case of an empty state - that is, for non-generative tests.
        // That's perfectly fine, we'll simply stop attempting to generate test cases.
        minWithIndex(values)
          .map: min =>
            Rand.State(values.updated(min.index, min.value + 1))

      override def execute(conf: Conf, test: (Rand, Params, Size, Assert) ?=> Unit) =
        // Runs the test using the specified state.
        // Note how we use `conf.maxSize` for size. This is a bit of a hack, and there must be a way of starting from
        // the min size and increasing that based on some criteria. But, for the time being, it's better to make the
        // size as large as possible to prevent cases where:
        // - the min size is 0
        // - some list generator uses that as the upper bound for its length
        // - even if the `Rand` handler pulls something bigger than 0, it will be normalised to 0
        // - the state will keep looping on this, because even though it keeps incrementing that value to 1, what will
        //   actually be recorded is 0.
        def run(state: Rand.State) =
          Size(conf.maxSize):
            Rand.replay(state):
              Rand.record:
                runTest(test)

        // Attempts to fail `test` by increasing `state` little by little. This will stop as soon as
        // `conf.minSuccess` is reached.
        @annotation.tailrec
        def loop(count: Int, state: Rand.State): Outcome =
          run(state) match
            case Rand.Recorded(Params.Recorded(Assertion.Success, _), state) =>
              if count >= conf.minSuccess - 1 then Outcome(count + 1, Result.Success)
              else
                nextState(state) match
                  case Some(state) => loop(count + 1, state)
                  case None        => Outcome(count + 1, Result.Success)

            case Rand.Recorded(Params.Recorded(Assertion.Failure(msg), params), state) =>
              Outcome(count, Result.Failure(msg, ReplayState(state, conf.maxSize), params))

        loop(0, Rand.State())

    body
