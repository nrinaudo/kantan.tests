package kantan.tests

import caps.*

// ---------------------------------------------------------------------------------------------------------------------
//
// Test configuration.
//
// While admittedly maybe slightly overkill, this makes it convenient to write test prompts that change the default
// configuration - by reducing the number of requested successes for an expensive test, for example.
//
// ---------------------------------------------------------------------------------------------------------------------

/** Capability that describes the ability to interract with a test's configuration. */
trait Conf extends SharedCapability:
  def get: Configuration
  def set(c: Configuration): Unit

object Conf:
  def get: Conf ?-> Configuration          = handler ?=> handler.get
  def set(c: Configuration): Conf ?-> Unit = handler ?=> handler.set(c)

  val minSize: Conf ?-> Int               = get.minSize
  def minSize_=(s: Int): Conf ?-> Unit    = set(get.copy(minSize = s))
  val maxSize: Conf ?-> Int               = get.maxSize
  def maxSize_=(s: Int): Conf ?-> Unit    = set(get.copy(maxSize = s))
  val minSuccess: Conf ?-> Int            = get.minSuccess
  def minSuccess_=(s: Int): Conf ?-> Unit = set(get.copy(minSuccess = s))

case class Configuration(minSuccess: Int, minSize: Int, maxSize: Int)
