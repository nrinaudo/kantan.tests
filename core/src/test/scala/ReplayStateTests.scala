package kantan.tests

import kantan.tests.Assert.*

object ReplayStateTests extends TestSuite:
  test("round-trip"):
    val state = ReplayState(
      randState = Rand.State(Rand.listOf(Rand.int(100))),
      size = Rand.int(100)
    )

    val observed = ReplayState.decode(ReplayState.encode(state))
    val expected = Some(state)

    assertEquals(observed, expected)
