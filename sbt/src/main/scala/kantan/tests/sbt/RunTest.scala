package kantan.tests.sbt

import kantan.tests.{Log as KLog, *}
import _root_.sbt.testing.*

// ---------------------------------------------------------------------------------------------------------------------
//
// Provides a `Run` handler that integrates with SBT.
//
// ---------------------------------------------------------------------------------------------------------------------

object RunTest:
  def apply[A](conf: Conf)(body: Run ?=> A)(using Log, Notify, Time): A =
    given Run:
      override def run(name: String, test: (Assert, KLog, Rand, Size) ?=> Unit, plan: Plan) =
        Time.mark
        plan.execute(test, conf) match
          case TestResult.Skipped(msg) =>
            Notify.skip

            Log.log(s"* [SKIPPED] $name", Color.Yellow)
            Log.log(s"  Message:  $msg", Color.Yellow)

          case TestResult.Success(successCount) =>
            Notify.success

            Log.log(s"* $name ($successCount successful attempt(s))", Color.Green)

          case TestResult.Failure(FailingTestCase(msg, replay, inputs, logs), shrinkCount, successCount) =>
            Notify.fail

            val inputMap = inputs.toMap
            Log.log(s"* $name ($successCount successful attempt(s))", Color.Red)
            Log.log(s"  Error: $msg", Color.Red)
            if inputMap.nonEmpty then
              Log.log(s"  Inputs (shrunk $shrinkCount time(s)):", Color.Red)
              inputMap.foreach:
                case (name, value) => Log.log(s"    - $name = $value", Color.Red)
            Log.log(s"  Replay: ${replay.encode}", Color.Red)

    body
