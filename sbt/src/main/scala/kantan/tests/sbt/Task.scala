package kantan.tests.sbt

import caps.*
import kantan.tests.{Log as _, *}
import _root_.sbt.testing.{Task as SbtTask, *}
import scala.util.{Failure, Success, Try}

// ---------------------------------------------------------------------------------------------------------------------
//
// kantan.tests-specific implementation of SBT's `Task`.
//
// ---------------------------------------------------------------------------------------------------------------------

class Task(override val taskDef: TaskDef, conf: Conf, loader: ClassLoader) extends SbtTask:
  override val tags = Array.empty[String]

  val fullyQualifiedName: String = taskDef.fullyQualifiedName

  // This is essentially lifted from `verify`, don't ask me.
  private def loadTestSuite: Try[TestSuite] =
    Try:
      Class
        .forName(taskDef.fullyQualifiedName + "$", false, loader)
        .getField("MODULE$")
        .get(null)
        .asInstanceOf[TestSuite]

  override def execute(handler: EventHandler, loggers: Array[Logger]): Array[SbtTask] =
    Notify(handler, taskDef):
      Log(loggers):
        Time:
          loadTestSuite match
            case Success(testSuite) =>
              Log.log(s"$fullyQualifiedName:", Color.Green)
              RunTest(conf):
                testSuite.run

            case Failure(error) =>
              Notify.error(error)

              Log.log(s"$fullyQualifiedName:", Color.Red)
              Log.log(error.getMessage, Color.Red)

    Array.empty
