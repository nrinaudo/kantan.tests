package kantan.tests

trait TestSuite:
  def conf: Configuration = Configuration(100, 0, 100)

  private def printOutcome(desc: String, outcome: TestOutcome) =
    outcome match
      case TestOutcome(count, seed, Result.Success) =>
        print(Console.GREEN)
        println(s"* ${desc} ($count successful attempt(s))")

      case TestOutcome(count, seed, Result.Failure(shrinkCount, msg, params)) =>
        print(Console.RED)
        println(s"* ${desc} ($count successful attempt(s))")
        println(s"  Seed:  $seed")
        println(s"  Error: $msg")
        if params.values.nonEmpty then
          println(s"  Parameters (shrunk $shrinkCount time(s)):")
          params.values.foreach:
            case (name, value) => println(s"    - $name = $value")
    print(Console.RESET)

  def testNoShrink(desc: String)(body: (Rand, Params, Size, Assert) ?=> Unit): Unit =
    Shrink.noop:
      printOutcome(desc, execute(conf, body))

  def test(desc: String)(body: (Rand, Params, Size, Assert) ?=> Unit): Unit =
    Shrink:
      printOutcome(desc, execute(conf, body))
