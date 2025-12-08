package kantan.tests

trait TestSuite:
  def conf: Configuration = Configuration(100, 0, 100)

  def test(desc: String)(body: (Rand, Params, Size, Assert) ?=> Unit): Unit =
    val result = execute(conf, body)

    result.result match
      case Result.Success =>
        print(Console.GREEN)
        println(s"* ${desc} (${result.successCount} successful attempt(s))")

      case Result.Failure(shrinkCount, msg, params) =>
        print(Console.RED)
        println(s"* ${desc} (${result.successCount} successful attempt(s))")
        println(s"  Seed:  ${result.seed}")
        println(s"  Error: $msg")
        if params.values.nonEmpty then
          println(s"  Parameters (shrunk $shrinkCount time(s)):")
          params.values.foreach:
            case (name, value) => println(s"    - $name = $value")
    print(Console.RESET)
