package kantan.tests

trait TestSuite:
  def conf: Configuration = Configuration(100, 0, 100)

  private def printOutcome(desc: String, outcome: TestOutcome) =
    outcome match
      case TestOutcome(count, seed, Result.Success) =>
        print(Console.GREEN)
        println(s"* $desc ($count successful attempt(s))")

      case TestOutcome(count, seed, Result.Failure(shrinkCount, size, msg, params)) =>
        print(Console.RED)
        println(s"* $desc ($count successful attempt(s))")
        println(s"  Seed:  $seed")
        println(s"  Size:  $size")
        println(s"  Error: $msg")
        if params.values.nonEmpty then
          println(s"  Parameters (shrunk $shrinkCount time(s)):")
          params.values.foreach:
            case (name, value) => println(s"    - $name = $value")
    print(Console.RESET)

  private def printResult(desc: String, result: Result) =
    result match
      case Result.Success =>
        print(Console.GREEN)
        println(s"* $desc")

      case Result.Failure(shrinkCount, _, msg, params) =>
        print(Console.RED)
        println(s"* ${desc}")
        println(s"  Error: $msg")
        if params.values.nonEmpty then
          println(s"  Parameters (shrunk $shrinkCount time(s)):")
          params.values.foreach:
            case (name, value) => println(s"    - $name = $value")
    print(Console.RESET)


  def testNoShrink(desc: String)(body: (Rand, Params, Size, Assert) ?=> Unit): Unit =
    Shrink.noop:
      printOutcome(desc, execute(conf, body))

  def test(desc: String, size: Int, seed: Long)(body: (Rand, Params, Size, Assert) ?=> Unit): Unit =
    Size(size):
      Rand.withSeed(seed):
        printResult(desc, runOne(body))

  def test(desc: String)(body: (Rand, Params, Size, Assert) ?=> Unit): Unit =
    Shrink:
      printOutcome(desc, execute(conf, body))
