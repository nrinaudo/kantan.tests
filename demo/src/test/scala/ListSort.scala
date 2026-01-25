package kantan.tests.demo

import kantan.tests.*
import kantan.tests.Prompt.*
import kantan.tests.Assert.*

/** Tests a hand-rolled list sorting implementation.
  *
  * This demonstrates how to move from an exampe-based test to a generative one with very little work.
  */
object ListSort extends TestSuite:
  // Bubble sort implementation I copied off of https://scala.algorithmexamples.com/web/Sort/BubbleSort.html
  def sort(list: List[Int]): List[Int] =
    def sort(as: List[Int], bs: List[Int]): List[Int] =
      if as.isEmpty then bs
      else bubble(as, Nil, bs)

    def bubble(as: List[Int], zs: List[Int], bs: List[Int]): List[Int] = as match
      case h1 :: h2 :: t =>
        if h1 > h2 then bubble(h1 :: t, h2 :: zs, bs)
        else bubble(h2 :: t, h1 :: zs, bs)
      case h1 :: Nil => sort(zs, h1 :: bs)
      case Nil       => Nil

    sort(list, Nil)

  // Simple example based test.
  test("sort([4, 3, 2, 1]) = [1, 2, 3, 4]"):
    val input    = List(4, 3, 2, 1)
    val observed = sort(input)
    val expected = List(1, 2, 3, 4)

    assertEquals(expected, observed)

  // Generative test: our implementation and the standard one yield the same result on any list.
  // We transformed the example-based one into this one simply by replacing the hard-coded input into a random one.
  test("forall input, sort(input) == input.sorted"):
    val input    = Rand.listOf(Rand.int(100)).logAs("input")
    val observed = sort(input)
    val expected = input.sorted

    assertEquals(expected, observed)
