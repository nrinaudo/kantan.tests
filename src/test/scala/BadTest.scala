package kantan.tests

import Runner.*
import Assert.*

/** Shows how example-based tests can be misleading, and how generative ones catch incorrect assumptions.
  *
  * We also show how to easily replay a failing test case.
  */
object BadTest extends TestSuite:
  // Obviously incorrect assumption, not caught by a basic example-based test.
  test("Strings have odd lengths"):
    val input = "foo"

    Assert.assert(input.length % 2 != 0, s"'$input's length was even")

  // Making that test generative catches the problem immediately.
  test("Strings have odd lengths (this should fail)"):
    val input = Params.param("Input", Rand.identifier)

    Assert.assert(input.length % 2 != 0, s"'$input's length was even")

  // Replays the previous test on a known failing configuration.
  // This won't shrink or try different sizes, but merely replay that one, single failing test case.
  replay("Strings have odd lengths (this should fail)")("H4sIAAAAAAAA_2JgYGBlgGBGBjQAAAAA__8="):
    val input = Params.param("Input", Rand.identifier)

    Assert.assert(input.length % 2 != 0, s"'$input's length was even")
