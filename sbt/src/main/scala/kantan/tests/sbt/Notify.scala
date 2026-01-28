package kantan.tests.sbt

import caps.*
import kantan.tests.*
import _root_.sbt.testing.*

// ---------------------------------------------------------------------------------------------------------------------
//
// Abstracts over SBT's `EventHandler`.
//
// This is really only there to factorize all the `Event` creation code, which is quite boilerplatey and unpleasant to
// write.
//
// ---------------------------------------------------------------------------------------------------------------------

/** Ability to notify SBT of events. */
trait Notify extends SharedCapability:
  def handle(status: Status, duration: Long, error: Option[Throwable]): Unit

object Notify:
  def apply[A](handler: EventHandler, definition: TaskDef)(body: Notify ?=> A): A =
    given Notify = (state, time, error) =>
      handler.handle(new Event:
        override val fingerprint        = definition.fingerprint
        override val fullyQualifiedName = definition.fullyQualifiedName
        override val selector           = definition.selectors.head
        override val status             = state
        override val duration           = time
        override val throwable          = error.map(e => new OptionalThrowable(e)).getOrElse(new OptionalThrowable()))

    body

  def notify(status: Status, duration: Long, error: Option[Throwable])(using handler: Notify, t: Time): Unit =
    handler.handle(status, duration, error)

  def error(error: Throwable)(using Notify, Time): Unit =
    notify(Status.Error, Time.elapsed, Some(error))

  def skip(using Notify, Time): Unit =
    notify(Status.Skipped, Time.elapsed, None)

  def success(using Notify, Time): Unit =
    notify(Status.Success, Time.elapsed, None)

  def fail(using Notify, Time): Unit =
    notify(Status.Failure, Time.elapsed, None)
