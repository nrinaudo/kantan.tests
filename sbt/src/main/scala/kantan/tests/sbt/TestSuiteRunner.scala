package kantan.tests.sbt

import kantan.tests.Conf
import _root_.sbt.testing.{Runner as SbtRunner, Task as SbtTask, TaskDef}

// ---------------------------------------------------------------------------------------------------------------------
//
// kantan.tests-specific implementation of SBT's `Runner`.
//
// ---------------------------------------------------------------------------------------------------------------------

final class TestSuiteRunner(
    override val args: Array[String],
    override val remoteArgs: Array[String],
    classLoader: ClassLoader
) extends SbtRunner:
  private val conf = Conf(100, 0, 100) // TODO: this should be extracted from args

  override def tasks(taskDefs: Array[TaskDef]): Array[SbtTask] = taskDefs.map(Task(_, conf, classLoader))
  override val done                                            = ""
