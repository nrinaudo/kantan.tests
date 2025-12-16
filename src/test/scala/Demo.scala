package kantan.tests

import Runner.*

class Demo extends TestSuite:

  def nthLast(n: Int, as: List[Char]): Option[Char] = Some('z')

  test("nthLast(n, list) == expected"):
    // Generates the various moving bits of the test case:
    // - `expected` is the character we expect to find.
    // - `n` is the index (from the end) at which we expect to find `expected`.
    // - `cs` is the list in which we expect to find `expected`.
    val n        = Rand.int(10)
    val tail     = Rand.list(n, Rand.lowerAscii)
    val head     = Rand.list(Rand.int(Size.size - n - 1), Rand.lowerAscii)
    val expected = Rand.lowerAscii
    val list     = head ++ (expected :: tail)

    // Reports the inputs we actually care about.
    Params.param("expected", expected)
    Params.param("n", n)
    Params.param("list", list)

    // Actual test.
    nthLast(n, list) match
      case None           => Assert.fail(s"Expected $expected, found nothing")
      case Some(observed) => Assert.assert(observed == expected, s"Expected $expected, found $observed")

  test("Random list has odd length"):
    val input = Rand.identifier

    Params.param("Input", input)

    Assert.assert(input.length % 2 != 0, s"'$input's length was even")

  test("Fixed list has odd length"):
    val input = "foo"

    Params.param("Input", input)

    Assert.assert(input.length % 2 != 0, s"'$input's length was even")

@main def run =
  given Runner = new ConsoleRunner(Configuration(100, 0, 100))

  (new Demo).run
