package kantan.tests

import caps.*

// ---------------------------------------------------------------------------------------------------------------------
//
// Test execution tools.
//
// ---------------------------------------------------------------------------------------------------------------------

/** Capability describing the ability to run a test.
  *
  * The test and its execution plan are separated to allow a runner, such as the one used in a `TestSuite`, to treat the
  * plan as more of a suggestion. We could then list all declared tests and interact with them more directly, such as
  * through a CLI, and try different search / shrink strategies.
  */
trait Run extends SharedCapability:
  def run(desc: String, test: (Rand, Params, Size, Assert) ?=> Unit, plan: Plan): Unit

object Run:
  // - Test results ----------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  case class Outcome(successCount: Int, result: Result)

  enum Result:
    case Success(params: Params.Values)
    case Failure(msg: String, shrinkCount: Int, replay: ReplayState, params: Params.Values)
    case Skipped(msg: String)

  // - Run DSL ---------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def run(desc: String, test: (Rand, Params, Size, Assert) ?=> Unit, plan: Plan): Run ?->{test} Unit =
    handler ?=> handler.run(desc, test, plan)
