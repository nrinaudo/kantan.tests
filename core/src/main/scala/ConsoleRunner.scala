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
  override def run(name: String, test: (Assert, Log, Rand, Size) ?=> Unit, plan: Plan) =
    plan.execute(test, conf) match
      case TestResult.Skipped(msg) =>
        print(Console.YELLOW)
        println(s"* [SKIPPED] $name")
        println(s"  Message:  $msg")

      case TestResult.Success(successCount) =>
        print(Console.GREEN)
        println(s"* $name ($successCount successful attempt(s))")

      case TestResult.Failure(FailingTestCase(msg, replay, inputs, logs), shrinkCount, successCount) =>
        val inputMap = inputs.toMap
        print(Console.RED)
        println(s"* $name ($successCount successful attempt(s))")
        println(s"  Error: $msg")
        if inputMap.nonEmpty then
          println(s"  Inputs (shrunk $shrinkCount time(s)):")
          inputMap.foreach:
            case (name, value) => println(s"    - $name = $value")
        println(s"  Replay: ${ReplayState.encode(replay)}")

    print(Console.RESET)
