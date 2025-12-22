package kantan.tests

import Prompt.default.*
import Assert.*

/** Based on a HedgeHog property that I noticed was usually poorly reduced by kantan.tests.
  *
  * The principle of what I'm trying to do here is:
  *   - generate random test cases (`cheapItems` and `expensiveItems` below).
  *   - run tests until a failing test case is found.
  *   - that failing test case tends to be very large and hard to work with, so we then apply test case reduction:
  *     making the test case smaller and smaller until we can't find a smaller one that still fails.
  *
  * Test case reduction is what we're trying to optimise. The way it currently works is:
  *   - we record all random numbers pulled while creating the test case. It's basically a `List[Int]`.
  *   - random generators are written with the understanding that the smaller the number(s) they pull, the smaller they
  *     should be. For example, the first number pulled by a list generator is the size of the list.
  *   - so what we do is try to make each element in the `List[Int]` smaller and see where that gets us.
  *
  * The current implementation can be found in `Shrink.scala`. It's a little naive, but works relatively well in most
  * cases.
  *
  * You can plug your own implementation in `shrinkState` and see how it performs by running `Test/run` in the SBT
  * shell.
  */
object ShrinkOptim extends TestSuite:
  // - System under test -----------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  // Our basic test, which should fail most of the time: as soon as the "expensive" order list is not empty, merging
  // adds a processing cost which the test fails to take into account.
  val total: (Rand, Params, Size, Assert) ?-> Unit =
    val cheapItems     = Params.param("cheapItems", order(cheap))
    val expensiveItems = Params.param("expensiveItems", order(expensive))

    assertEquals(merge(cheapItems, expensiveItems).total.value, cheapItems.total.value + expensiveItems.total.value)

  // This is commented out to make the output less noisy, but it's the standard way a test is ran (including current
  // shrinking).
  // test("total"):
  //   total

  // - tools to optimise shrinking -------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  // This is a recorded state - all random numbers that were pulled while running `total`, in the order in which they
  // were pulled.
  val failing = List(3, 0, 3, 0, 1, 0, 2, 34, 0, 334, 0, 852, 0, 550, 0, 215, 0, 921, 0, 786, 0, 408, 0, 482, 0, 979, 0, 56, 0, 928, 0, 292, 0, 750, 0, 226, 0, 61, 0, 783, 0, 301, 0, 368, 0, 615, 0, 919, 0, 419, 0, 140, 0, 919, 0, 908, 0, 512, 0, 872, 0, 368, 0, 260, 0, 485, 0, 570, 0, 450, 0, 346, 0, 12, 0, 873)

  // Takes a known failing state and shrinks it to a smaller one that's still failing.
  // This is obviously currently a noop.
  def shrinkState(state: List[Int]): List[Int] =
    state

  // This replays a known failing state after attempting to shrink it.
  // smaller.
  replay("total (replayed)")(ReplayState(Rand.State(shrinkState(failing)), 0)):
    total

  // - Data & generators used by the test ------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------

  case class USD(value: Long)
  case class Item(name: String, price: USD)
  case class Order(items: List[Item]):
    def total: USD =
      USD(items.map(_.price.value).sum)

  /** Merges two orders, adding processing costs for expensive ones. */
  def merge(xs: Order, ys: Order): Order =
    val extra =
      if xs.items.exists(_.price.value > 50) || ys.items.exists(_.price.value > 50) then
        List(Item("processing", USD(1)))
      else Nil
    Order(xs.items ++ ys.items ++ extra)

  /** Generates a random cheap item. */
  val cheap: Rand ?-> Item =
    val n = Rand.oneOf("sandwich", "noodles")
    val p = USD(Rand.range(5, 10))

    Item(n, p)

  /** Generates a random expensive item. */
  val expensive: Rand ?-> Item =
    val n = Rand.oneOf("oculus", "vive")
    val p = USD(Rand.range(1000, 2000))

    Item(n, p)

  /** Generates a random order of the specified items. */
  def order(gen: Rand ?=> Item): Rand ?->{gen} Order =
    Order(Rand.listOf(Rand.int(50), gen))
