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
  def run(desc: String, test: (Assert, Log, Rand, Size) ?=> Unit, plan: Plan): Unit

object Run:
  // - Run DSL ---------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def run(desc: String, test: (Assert, Log, Rand, Size) ?=> Unit, plan: Plan)(using handler: Run): Unit =
    handler.run(desc, test, plan)
