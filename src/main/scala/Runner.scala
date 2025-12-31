package kantan.tests

import caps.*

// ---------------------------------------------------------------------------------------------------------------------
//
// Test execution tools.
//
// ---------------------------------------------------------------------------------------------------------------------

/** Capability describing the ability to run a test. */
trait Runner extends SharedCapability:
  def run(desc: String, test: (Rand, Params, Size, Assert) ?=> Unit, plan: Plan): Unit

object Runner:
  // - Test results ----------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  case class Outcome(successCount: Int, result: Result)

  enum Result:
    case Success
    case Failure(msg: String, shrinkCount: Int, replay: ReplayState, params: Params.Values)
    case Skipped(msg: String)

    def isSuccess = this match
      case Success => true
      case _       => false

    def isFailure = !isSuccess

  // - Runner DSL ------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def run(desc: String, test: (Rand, Params, Size, Assert) ?=> Unit, plan: Plan): Runner ?->{test} Unit =
    handler ?=> handler.run(desc, test, plan)
