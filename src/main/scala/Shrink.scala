package kantan.tests

// ---------------------------------------------------------------------------------------------------------------------
//
// Test case reduction.
//
// The general idea is very simple: a test case is the result of pulling a series of random numbers. In order to shrink
// the test case, then, we'll shrink the series of random numbers, assuming that lower numbers lead to smaller values.
// This is very much inspired (although I only realised that after the fact) by David McIver's work on Hypothesis.
//
// ---------------------------------------------------------------------------------------------------------------------

trait Shrink:
  def shrink(state: Rand.State): LazyList[Rand.State]

object Shrink:
  // - Default shrinker ------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** Runs the specified computation using the default shrinker.
    *
    * This shrinks state by first shrinking the head towards 0 by halves, then shrinking the tail.
    */
  def apply[A](body: Shrink ?=> A): A =
    def halves(i: Int): LazyList[Int] =
      if i == 0 then LazyList.empty
      else
        val head = i / 2
        head #:: halves(head)

    def loop(state: List[Int]): LazyList[List[Int]] =
      state match
        case head :: tail =>
          halves(head).map(_ :: tail)
            #::: loop(tail).map(head :: _)
        case Nil => LazyList.empty

    given Shrink = state => loop(state.toInts).map(Rand.State.apply)

    body

  // - Noop shrinker ---------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** Provides a `Shrink` handler that doesn't actually shrink.
    *
    * This can be useful when you need to disable shrinking - it could be too expensive, for example, or produce too
    * much noise.
    */
  def noop[A](body: Shrink ?=> A): A =
    given Shrink = _ => LazyList.empty

    body

  // - Basic operations ------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def state(s: Rand.State): Shrink ?-> LazyList[Rand.State] =
    handler ?=> handler.shrink(s)

// - Actual shrinking --------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
/** Reruns the specified test on the specified state.
  *
  * This allows us to try shrunk states and see how they work out.
  */
private def runState(
    state: Rand.State,
    size: Int,
    body: (Rand, Params, Size, Assert) ?=> Unit
): Rand.Recorded[Result] =
  Size(size):
    Rand.replay(state):
      Rand
        .record(runTest(body))
        .map:
          case Params.Recorded(Assertion.Success, _)           => Result.Success
          case Params.Recorded(Assertion.Failure(msg), params) => Result.Failure(0, size, msg, params)

/** Shrinks the specified test, known to have failed with `failure` on state `state`. */
private def shrink(
    body: (Rand, Params, Size, Assert) ?=> Unit,
    failure: Result.Failure,
    state: Rand.State,
    size: Int
): Shrink ?=> Result.Failure =
  def loop(states: LazyList[Rand.State], failure: Result.Failure): Result.Failure = states match
    case head #:: tail =>
      runState(head, size, body) match
        case Rand.Recorded(Result.Success, _) =>
          loop(tail, failure)

        case Rand.Recorded(e: Result.Failure, state) =>
          loop(Shrink.state(state), e.copy(shrinkCount = e.shrinkCount + 1))

    case _ => failure

  loop(Shrink.state(state), failure)
