package kantan.tests

import Prompt.*

// ---------------------------------------------------------------------------------------------------------------------
//
// Test case reduction.
//
// The general idea is very simple: a test case is the result of pulling a series of random numbers. In order to shrink
// the test case, then, we'll shrink the series of random numbers, assuming that lower numbers lead to smaller values.
// This is very much inspired (although I only realised that after the fact) by David McIver's work on Hypothesis.
//
// ---------------------------------------------------------------------------------------------------------------------

case class Shrunk(result: Plan.Result.Failure, shrinkCount: Int)

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

  // - Caching shrinker ------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** Handler that caches previously seen test cases and avoids re-running them. */
  def caching[A](maxSize: Int)(body: Shrink ?=> A): Shrink ?->{body} A = handler ?=>
    val cache = new java.util.LinkedHashMap[Rand.State, Unit](16, 0.75, true):
      override def removeEldestEntry(eldest: java.util.Map.Entry[Rand.State, Unit]) = size > maxSize

    def isNew(state: Rand.State) =
      if cache.containsKey(state) then false
      else
        cache.put(state, ())
        true

    given Shrink = state => handler.shrink(state).filter(isNew)

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

  // - Actual shrinking ------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** Reruns the specified test on the specified state.
    *
    * This allows us to try shrunk states and see how they work out.
    */
  private def runState(
      state: Rand.State,
      size: Int,
      body: (Rand, Params, Size, Assert) ?=> Unit
  ): Rand.Recorded[Plan.Result] =
    Size(size):
      Rand.replay(state):
        Rand
          .record(runTest(body))
          .map:
            case Params.Recorded(Assertion.Success, _)           => Plan.Result.Success
            case Params.Recorded(Assertion.Failure(msg), params) =>
              Plan.Result.Failure(msg, ReplayState(state, size), params)

  /** Shrinks the specified test, known to have failed with `failure` on state `state`. */
  def shrink(
      body: (Rand, Params, Size, Assert) ?=> Unit,
      failure: Plan.Result.Failure
  ): Shrink ?->{body} Shrunk =
    def loop(states: LazyList[Rand.State], failure: Shrunk): Shrunk = states match
      case head #:: tail =>
        runState(head, failure.result.state.size, body) match
          case Rand.Recorded(Plan.Result.Success, _) =>
            loop(tail, failure)

          case Rand.Recorded(e: Plan.Result.Failure, state) =>
            loop(Shrink.state(state), Shrunk(e, failure.shrinkCount + 1))

      case _ => failure

    loop(Shrink.state(failure.state.state), Shrunk(failure, 0))
