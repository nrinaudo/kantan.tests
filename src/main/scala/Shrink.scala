package kantan.tests

// ---------------------------------------------------------------------------------------------------------------------
//
// Utilities for shrinking failing test cases.
//
// The general idea is very simple: a test case is the result of pulling a series of random numbers. In order to shrink
// the test case, then, we'll shrink the series of random numbers, assuming that lower numbers lead to smaller values.
// This is very much inspired (although I only realised that after the fact) by David McIver's work on Hypothesis.
//
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
  Rand.replay(state):
    Rand
      .record(runTest(size, body))
      .map:
        case Params.Recorded(Assertion.Success, _)           => Result.Success
        case Params.Recorded(Assertion.Failure(msg), params) => Result.Failure(0, msg, params)

private def shrink(
    body: (Rand, Params, Size, Assert) ?=> Unit,
    failure: Result.Failure,
    state: Rand.State,
    size: Int
): Result.Failure =
  def loop(states: LazyList[Rand.State], failure: Result.Failure): Result.Failure = states match
    case head #:: tail =>
      runState(head, size, body) match
        case Rand.Recorded(Result.Success, _) =>
          loop(tail, failure)

        case Rand.Recorded(e: Result.Failure, state) =>
          loop(shrinkState(state), e.copy(shrinkCount = e.shrinkCount + 1))

    case _ => failure

  loop(shrinkState(state), failure)

private def shrinkState(state: Rand.State): LazyList[Rand.State] =
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

  loop(state.toInts).map(Rand.State.apply)
