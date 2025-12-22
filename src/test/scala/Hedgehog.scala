package kantan.tests

import Prompt.default.*
import Assert.*

/** Shows how to rewrite the Hedgehog examples using kantan.tests. */
object Hedgehog extends TestSuite:
  test("example1"):
    val x = Params.param("x", Rand.lowerAscii)
    val y = Params.param("y", Rand.int(50))

    Assert.assert(y < 87 && x <= 'r')

  test("total"):
    val x = Params.param("x", order(cheap))
    val y = Params.param("y", order(expensive))

    assertEquals(merge(x, y).total.value, x.total.value + y.total.value)

  case class USD(value: Long)
  case class Item(name: String, price: USD)
  case class Order(items: List[Item]):
    def total: USD =
      USD(items.map(_.price.value).sum)

  def merge(xs: Order, ys: Order): Order =
    val extra =
      if xs.items.exists(_.price.value > 50) || ys.items.exists(_.price.value > 50) then
        List(Item("processing", USD(1)))
      else Nil
    Order(xs.items ++ ys.items ++ extra)

  val cheap: Rand ?-> Item =
    val n = Rand.oneOf("sandwich", "noodles")
    val p = USD(Rand.range(5, 10))

    Item(n, p)

  val expensive: Rand ?-> Item =
    val n = Rand.oneOf("oculus", "vive")
    val p = USD(Rand.range(1000, 2000))

    Item(n, p)

  def order(gen: Rand ?=> Item): (Size, Rand) ?->{gen} Order =
    Order(Rand.list(gen))
