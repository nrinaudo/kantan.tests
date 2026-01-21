package kantan.tests

import caps.*

// ---------------------------------------------------------------------------------------------------------------------
//
// Searching for failing test cases.
//
// ---------------------------------------------------------------------------------------------------------------------

// - Search capability -------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
/** Describes the ability to search for a failing test case for a given test. */
trait Search extends SharedCapability:
  def search(conf: Conf, test: (Assert, Log, Rand, Size) ?=> Unit): Search.Result

object Search:

  // - Search results --------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  case class Result(testCase: Option[FailingTestCase], successCount: Int)

  object Result:
    def failure(
        msg: String,
        randState: Rand.State,
        size: Int,
        successCount: Int,
        inputs: Log.Inputs,
        logs: Log.Entries
    ): Result =
      Result(Some(FailingTestCase(msg, ReplayState(randState, size), inputs, logs)), successCount)

    def success(successCount: Int): Result =
      Result(None, successCount)

  // - Search execution ------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def search(conf: Conf, test: (Assert, Log, Rand, Size) ?=> Unit)(using handler: Search): Result =
    handler.search(conf, test)

  // - Test case growth ------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** Searches through the test space by generating increasingly large test cases.
    *
    * This is done by playing on two parameters:
    *   - the test case's `size` (as controlled by `Size`), which increases linearly from `minSize` to `maxSize`.
    *   - the maximum value pulled by the random generator, which increases geometrically from `1` to `Int.MaxValue`.
    *
    * This is heavily inspired by QuickCheck and similar tools.
    */
  def grow[A](body: Search ?=> A): A =
    given Search:
      def run(size: Int, randCeiling: Double, test: (Assert, Log, Rand, Size) ?=> Unit) =
        Size(size):
          Rand:
            Rand.bound(randCeiling.toInt):
              Rand.record:
                runTest(test)

      override def search(conf: Conf, test: (Assert, Log, Rand, Size) ?=> Unit) =
        val sizeStep = (conf.maxSize - conf.minSize) / conf.minSuccess
        val randStep = math.pow(Int.MaxValue, 1.0 / conf.minSuccess)

        def loop(successCount: Int, size: Int, randCeiling: Double): Result =
          run(size, randCeiling, test) match
            case Rand.Recorded(Log.Recorded(AssertionResult.Success, _, _), state) =>
              // If the state is empty, this is not a random-based test and there's no need to try it more than once.
              // If it *is* a random-based test, but we've ran it enough times, then the test is successful.
              val success = state.isEmpty || successCount >= conf.minSuccess - 1

              if success then Result.success(successCount + 1)
              else loop(successCount + 1, size + sizeStep, randCeiling * randStep)

            case Rand.Recorded(Log.Recorded(AssertionResult.Failure(msg), inputs, logs), randState) =>
              Result.failure(msg, randState, size, successCount, inputs, logs)

        loop(0, conf.minSize, randStep)

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
      case class Min(index: Int, value: Int):
        def or(newIndex: Int, newValue: Int): Min =
          if newValue <= value then Min(newIndex, newValue)
          else this

      // Finds the smallest value in the specified list of ints (right-leaning).
      def minWithIndex(is: List[Int]): Option[Min] =
        def go(i: Int, min: Min, is: List[Int]): Min =
          is match
            case head :: tail => min.or(i, head)
            case _            => min

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

      override def search(conf: Conf, test: (Assert, Log, Rand, Size) ?=> Unit) =
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
            case Rand.Recorded(Log.Recorded(AssertionResult.Success, _, _), state) =>
              if count >= conf.minSuccess - 1 then Result(None, count + 1)
              else
                nextState(state) match
                  case Some(state) => loop(count + 1, state)
                  case None        => Result.success(count + 1)

            case Rand.Recorded(Log.Recorded(AssertionResult.Failure(msg), inputs, logs), state) =>
              Result.failure(msg, state, conf.maxSize, count, inputs, logs)

        loop(0, Rand.State())

    body
