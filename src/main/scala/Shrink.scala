package kantan.tests

import caps.*
import Prompt.*

// ---------------------------------------------------------------------------------------------------------------------
//
// Test case reduction.
//
// ---------------------------------------------------------------------------------------------------------------------

/** Describes the ability to reduce a failing test case to a smaller one.
  *
  * Note that there is no guarantee that the shrunk test case will be the smallest possible, nor that one will be found
  * at all. This is merely a best attempt.
  *
  * The API for this is designed with experimentation in mind, allowing "easy" implementation of wacky ideas. At the
  * time of writing, there really is only one useful implementation: `Shrink.Naive`.
  */
trait Shrink extends SharedCapability:
  def shrink(test: (Rand, Params, Size, Assert) ?=> Unit, failure: FailingCase): Shrink.Result

object Shrink:
  // - Shrink result ---------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** Contains the smallest failing test case we could find, with the number of times it was shrunk. */
  case class Result(failure: FailingCase, shrinkCount: Int)

  // - Basic operations ------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** Shrinks the specified test failure. */
  def shrink(
      test: (Rand, Params, Size, Assert) ?=> Unit,
      failure: FailingCase
  ): Shrink ?->{test} Shrink.Result =
    handler ?=> handler.shrink(test, failure)

  // - Naive shrinking -------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** Shrinkers based on a naive exploration of the test space.
    *
    * Naive shrinkers take a state and generate a list of interesting states to look at from it. They then explore all
    * of them until they find a new failing one, and start the process over.
    */
  trait Naive extends Shrink:
    def shrink(state: Rand.State): LazyList[Rand.State]

    // The concept here is very simple: the state shrinker generates all interesting states to look at, ordered from
    // most to least interesting. We'll then explore them, in that order, until we either run out of states, or find
    // a new failing one. We'll then attempt to shrink that one.
    override def shrink(test: (Rand, Params, Size, Assert) ?=> Unit, failure: FailingCase) =
      def loop(states: LazyList[Rand.State], shrunk: Shrink.Result): Shrink.Result = states match
        case head #:: tail =>
          Naive.runState(head, shrunk.failure.state.size, test) match
            case Rand.Recorded(None, _) =>
              loop(tail, shrunk)

            case Rand.Recorded(Some(e), state) =>
              loop(shrink(state), Shrink.Result(e, shrunk.shrinkCount + 1))

        case _ => shrunk

      loop(shrink(failure.state.state), Shrink.Result(failure, 0))

  object Naive:
    def shrink(state: Rand.State): Naive ?-> LazyList[Rand.State] = handler ?=> handler.shrink(state)

    // - Helpers ---------------------------------------------------------------------------------------------------------
    // -------------------------------------------------------------------------------------------------------------------
    /** Runs the specified test on the specified state.
      *
      * This allows us to try shrunk states and see how they work out.
      */
    def runState(
        state: Rand.State,
        size: Int,
        test: (Rand, Params, Size, Assert) ?=> Unit
    ): Rand.Recorded[Option[FailingCase]] =
      Size(size):
        Rand.replay(state):
          Rand
            .record(runTest(test))
            .map:
              case Params.Recorded(Assertion.Success, _)           => None
              case Params.Recorded(Assertion.Failure(msg), params) =>
                Some(FailingCase(msg, ReplayState(state, size), params))

    /** Adds an LRU-cache to an existing naive shrinker, to avoid revisiting known states. */
    def caching[A](maxSize: Int)(body: Naive ?=> A): Naive ?->{body} A = handler ?=>
      val cache = new java.util.LinkedHashMap[Rand.State, Unit](16, 0.75, true):
        override def removeEldestEntry(eldest: java.util.Map.Entry[Rand.State, Unit]) = size > maxSize

      def isNew(state: Rand.State) =
        if cache.containsKey(state) then false
        else
          cache.put(state, ())
          true

      given Naive = state => shrink(state).filter(isNew)

      body

    /** Runs the specified computation using a naive shrinker. */
    def apply[A](body: Naive ?=> A): A =
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

      given Naive = state => loop(state.toInts).map(Rand.State.apply)

      body

  // - Noop shrinker ---------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** Provides a `Shrink` handler that doesn't actually shrink.
    *
    * This can be useful when you need to disable shrinking - it could be too expensive, for example, or produce too
    * much noise.
    */
  def noop[A](body: Shrink ?=> A): A =
    given Shrink = (_, failure) => Shrink.Result(failure, 0)

    body
