package kantan.tests

import caps.*

// ---------------------------------------------------------------------------------------------------------------------
//
// Random value generation.
//
// All generators are based on the ability to generate a randon number between `0` and `n`. Different handlers are
// provided, most notably offering ways to record all generated numbers and replay them later.
//
// ---------------------------------------------------------------------------------------------------------------------

/** The ability to generate random numbers, from which all other random generators are derived. */
trait Rand extends SharedCapability:
  def nextInt(max: Int): Int

object Rand:
  // - Basic random number generator -----------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** Runs the specified computation using the standard [[scala.util.Random]] random number generator, initialised on a
    * random seed.
    */
  def apply[A](a: Rand ?=> A): A =
    withSeed(scala.util.Random.nextLong)(a)

  /** Runs the specified computation using the standard [[scala.util.Random]] random number generator, initialised on
    * the specified seed.
    */
  def withSeed[A](seed: Long)(body: Rand ?=> A): A =
    val rand = scala.util.Random(seed)

    given Rand = (max: Int) => if max <= 0 then 0 else rand.nextInt(max)

    body

  // - Bound generator -------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** Random generator that can never pull numbers higher than the specified upper boundary.
    *
    * This is useful to limit the size of generated test cases, for example.
    */
  def bound[A](upper: Int)(body: Rand ?=> A): Rand ?->{body} A = handler ?=>
    given Rand = (max: Int) => handler.nextInt(math.min(upper, max))

    body

  // - Random state recording ------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  case class Recorded[A](value: A, state: State):
    def map[B](f: A => B): Recorded[B] = copy(value = f(value))

  /** Random generator state after a test has been ran - the series of random numbers that were pulled. */
  opaque type State = List[Int]

  object State:
    extension (state: State)
      inline def isEmpty: Boolean  = state.isEmpty
      inline def toInts: List[Int] = state

    inline def apply(state: List[Int]): State = state
    inline def apply(state: Int*): State      = state.toList

  /** Records every generated random number while running the specified computation.
    *
    * This is particularly useful for test case reduction.
    */
  def record[A](body: Rand ?=> A): Rand ?->{body} Recorded [A] = r ?=>
    val state = List.newBuilder[Int]

    given Rand = (max: Int) =>
      val value = r.nextInt(max)
      state += value
      value

    Recorded(body, state.result)

  // - Random state replaying ------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** Replays the specified series of generated random numbers.
    *
    * This is particularly useful for test case reduction.
    */
  def replay[A](state: State)(body: Rand ?=> A): A =
    var current = state

    given Rand = (max: Int) =>
      current match
        case rand :: rest =>
          current = rest

          // Returns the last generated number, making sure it fits the desired interval.
          if rand < 0 then 0
          else if rand > max then max
          else rand

        case Nil => 0

    body

// - Basic generators --------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------

  def int(max: Int): Rand ?-> Int =
    rand ?=> rand.nextInt(math.max(0, max))

  def range(min: Int, max: Int): Rand ?-> Int =
    int(max - min) + min

  val boolean: Rand ?-> Boolean =
    int(2) == 1

  def range(min: Char, max: Char): Rand ?-> Char =
    range(min.toInt, max.toInt).toChar

  val lowerAscii: Rand ?-> Char =
    range('a', 'z')

  val upperAscii: Rand ?-> Char =
    range('A', 'Z')

  val digit: Rand ?-> Char =
    range('0', '9')

  def oneOf[A, Tail ^](head: Rand ?=> A, tail: (Rand ?->{Tail} A)*): Rand ?->{head, Tail} A =
    val index = int(tail.length)

    if index == 0 then head
    else tail(index - 1)

  val size: (Size, Rand) ?-> Int = int(Size.size)

  def listOf[A](length: Int, content: Rand ?=> A): Rand ?->{content} List [A] =
    val builder = List.newBuilder[A]

    (0 until length).foreach: _ =>
      builder += content

    builder.result

  def list[A](content: Rand ?=> A): (Rand, Size) ?->{content} List [A] =
    listOf(size, content)

  val identifier: (Size, Rand) ?-> String =
    // Always generate the things with the most influence earlier, as it helps with shrinking.
    val tail = listOf(int(Size.size - 1), oneOf(lowerAscii, upperAscii, digit, '_'))
    val head = oneOf(lowerAscii, upperAscii, '_')

    (head :: tail).mkString

  def resize[A](s: Int, content: (Rand, Size) ?=> A): (Rand, Size) ?->{content} A =
    Size.fork:
      Size.size = s
      content
