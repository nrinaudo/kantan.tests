package kantan.tests

import caps.*

// ---------------------------------------------------------------------------------------------------------------------
//
// Exposes the size of generated test values.
//
// The notion of size is left to the interpretation of each generator - it could be, for example, the maximum length of
// a list, or the odds of getting a `Some` when generating options.
//
// ---------------------------------------------------------------------------------------------------------------------

/** Capability exposing a size value that can be queried and updated.
  *
  * It's essentially specialized mutable state.
  */
trait Size extends SharedCapability:
  def get: Int
  def set(s: Int): Unit

object Size:
  def apply[A](initial: Int)(body: Size ?=> A): A =
    var size = initial

    given Size:
      override def get         = size
      override def set(s: Int) = size = s

    body

    /** Queries the current size. */
  def size(using handler: Size): Int = handler.get

  /** Updates the current size.
    *
    * Note that you sometimes want to update the size _for a group of statements only_. In that case, it's better to use
    * `Size.fork`.
    */
  def size_=(s: Int)(using handler: Size): Unit = handler.set(math.max(0, s))

  /** Runs the specified effectful computation, allowing it to modify the size at will, but restores it to its initial
    * value when done.
    */
  def fork[A](body: Size ?=> A)(using Size): A =
    val oldSize = size
    val result  = body

    size = oldSize
    result

  def resize[A](s: Int)(content: Size ?=> A)(using Size): A =
    Size.fork:
      Size.size = s
      content
