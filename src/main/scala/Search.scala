package kantan.tests

import caps.*
import Prompt.*

// ---------------------------------------------------------------------------------------------------------------------
//
// Searching for failing test cases.
//
// ---------------------------------------------------------------------------------------------------------------------

// - Helper types ------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------

case class Conf(minSuccess: Int, minSize: Int, maxSize: Int)

case class FailingCase(msg: String, state: ReplayState, params: Params.Values)

// - Test plan ---------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
/** Describes the ability to search for a failing test case for a given test. */
trait Search extends SharedCapability:
  def search(conf: Conf, test: (Rand, Params, Size, Assert) ?=> Unit): Search.Result

object Search:

  // - Search results --------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  case class Result(successCount: Int, failure: Option[FailingCase])

  // - Search execution ------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def search(conf: Conf, test: (Rand, Params, Size, Assert) ?=> Unit): Search ?->{test} Result =
    handler ?=> handler.search(conf, test)

  // - Linear growth ---------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** Searches through the test space by increasing `size` linearly from the minimum allowed value to the maximum one.
    *
    * This is heavily inspired by QuickCheck and other members of the XXXCheck family of generative test libraries.
    */
  def linearGrowth[A](body: Search ?=> A): A =
    given Search:
      // Runs the specified test on the specified size.
      def run(size: Int, test: (Rand, Params, Size, Assert) ?=> Unit) =
        Size(size):
          Rand:
            Rand.record:
              runTest(test)

      override def search(conf: Conf, test: (Rand, Params, Size, Assert) ?=> Unit) =
        val sizeStep = (conf.maxSize - conf.minSize) / conf.minSuccess

        def loop(count: Int, size: Int): Result =
          run(size, test) match
            case Rand.Recorded(Params.Recorded(Assertion.Success, _), state) =>
              // If the state is empty, this is not a random-based test and there's no need to try it more than once.
              // If it *is* a random-based test, but we've ran it enough times, then the test is successful.
              val success = state.isEmpty || count >= conf.minSuccess - 1

              if success then Result(count + 1, None)
              else loop(count + 1, size + sizeStep)

            case Rand.Recorded(Params.Recorded(Assertion.Failure(msg), params), state) =>
              Result(count, Some(FailingCase(msg, ReplayState(state, size), params)))

        loop(0, conf.minSize)

    body

  // - Enumeration -----------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** Searches through the test space by enumerating all possible test cases, starting from the smallest possible one.
    *
    * This will stop as soon as `minSuccess` is reached.
    *
    * This is heavily inspired by `SmallCheck and Lazy SmallCheck`, which reasons that most tests that fail, do so on
    * some very small test cases.
    *
    * This is probably not the right default search, but is likely a good one to use in a more interactive mode -
    * playing maybe by starting a test with this plan to find all the low hanging fruits, and then move to one that
    * casts a wider net.
    */
  def enumerate[A](body: Search ?=> A): A =
    given Search:

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

      override def search(conf: Conf, test: (Rand, Params, Size, Assert) ?=> Unit) =
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
        def loop(count: Int, state: Rand.State): Result =
          run(state) match
            case Rand.Recorded(Params.Recorded(Assertion.Success, _), state) =>
              if count >= conf.minSuccess - 1 then Result(count + 1, None)
              else
                nextState(state) match
                  case Some(state) => loop(count + 1, state)
                  case None        => Result(count + 1, None)

            case Rand.Recorded(Params.Recorded(Assertion.Failure(msg), params), state) =>
              Result(count, Some(FailingCase(msg, ReplayState(state, conf.maxSize), params)))

        loop(0, Rand.State())

    body
