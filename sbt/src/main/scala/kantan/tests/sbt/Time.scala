package kantan.tests.sbt

import caps.*

// ---------------------------------------------------------------------------------------------------------------------
//
// Provides a simple (simplistic?) way of tracking time.
//
// This probably doesn't work all that well in a multithreaded context, I might need to rethink this if I ever support
// parallel tests.
//
// ---------------------------------------------------------------------------------------------------------------------

/** Simple capability used to time executions. */
trait Time extends SharedCapability:
  def elapsed(): Long
  def mark(): Unit

object Time:
  def apply[A](body: Time ?=> A): A =
    var start = System.currentTimeMillis()

    given Time:
      override def elapsed() = System.currentTimeMillis - start
      override def mark()    = start = System.currentTimeMillis

    body

  def mark(using handler: Time): Unit    = handler.mark()
  def elapsed(using handler: Time): Long = handler.elapsed()
