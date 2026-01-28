package kantan.tests

import java.io.{PrintStream, PrintWriter}

import sbt.testing.{Fingerprint, Framework}

class SbtFramework extends Framework:

  override def name() = "kantan.tests"

  override def fingerprints() = ???

  override def runner(args: Array[String], remoteArgs: Array[String], testClassLoader: ClassLoader) = ???

  def slaveRunner(args: Array[String], remoteArgs: Array[String], testClassLoader: ClassLoader, send: String => Unit) =
    ???
