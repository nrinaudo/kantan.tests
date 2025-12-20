package kantan.tests

// ---------------------------------------------------------------------------------------------------------------------
//
// Basic `Runner`.
//
// This is more intended for demonstration purposes than anything else.
//
// ---------------------------------------------------------------------------------------------------------------------

/** Simple runner that reports tests to the standard output. */
class ConsoleRunner(conf: Configuration) extends Runner:
  def run(name: String, body: Configuration => TestOutcome) =
    body(conf) match
      case TestOutcome(count, Result.Skipped(msg)) =>
        print(Console.YELLOW)
        println(s"* [SKIPPED] $name ($count successful attempt(s))")

      case TestOutcome(count, Result.Success) =>
        print(Console.GREEN)
        println(s"* $name ($count successful attempt(s))")

      case TestOutcome(count, Result.Failure(shrinkCount, msg, replay, params)) =>
        print(Console.RED)
        println(s"* $name ($count successful attempt(s))")
        println(s"  Error: $msg")
        if params.values.nonEmpty then
          println(s"  Parameters (shrunk $shrinkCount time(s)):")
          params.values.foreach:
            case (name, value) => println(s"    - $name = $value")
        println(s"  Replay: ${replay.encode}")

    print(Console.RESET)
