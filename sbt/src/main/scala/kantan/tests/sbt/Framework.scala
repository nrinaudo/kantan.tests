package kantan.tests.sbt

import _root_.sbt.testing.{Framework as SbtFramework, Runner as SbtRunner, *}

// ---------------------------------------------------------------------------------------------------------------------
//
// kantan.tests-specific implementation of SBT's `Framework`.
//
// ---------------------------------------------------------------------------------------------------------------------

/** Interface between SBT and kantan.tests. */
class Framework extends SbtFramework:
  override val name = "kantan.tests"

  override val fingerprints = Array[Fingerprint](new SubclassFingerprint:
    override val isModule                = true
    override val requireNoArgConstructor = true
    override val superclassName          = "kantan.tests.TestSuite")

  override def runner(args: Array[String], remoteArgs: Array[String], testClassLoader: ClassLoader): SbtRunner =
    TestSuiteRunner(args, remoteArgs, testClassLoader)
