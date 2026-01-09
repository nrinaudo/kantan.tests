package kantan.tests

// ---------------------------------------------------------------------------------------------------------------------
//
// Basic `Run`.
//
// This is more intended for demonstration purposes than anything else.
//
// ---------------------------------------------------------------------------------------------------------------------

/** Simple runner that reports tests to the standard output. */
class ConsoleRunner(conf: Conf) extends Run:
  override def run(name: String, test: (Rand, Params, Size, Assert) ?=> Unit, plan: Plan) =
    plan.execute(test, conf) match
      case TestResult.Skipped(msg) =>
        print(Console.YELLOW)
        println(s"* [SKIPPED] $name")
        println(s"  Message:  $msg")

      case TestResult.Ran(successCount, params, TestResult.Status.Success) =>
        print(Console.GREEN)
        println(s"* $name (successCount successful attempt(s))")

      case TestResult.Ran(successCount, params, TestResult.Status.Failure(msg, shrinkCount, replay)) =>
        print(Console.RED)
        println(s"* $name ($successCount successful attempt(s))")
        println(s"  Error: $msg")
        if params.values.nonEmpty then
          println(s"  Parameters (shrunk $shrinkCount time(s)):")
          params.values.foreach:
            case (name, value) => println(s"    - $name = $value")
        println(s"  Replay: ${replay.encode}")

    print(Console.RESET)
