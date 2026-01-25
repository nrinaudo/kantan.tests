package kantan.tests

import caps.*
import collection.Factory
import collection.immutable.TreeMap

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
  def bound[A](upper: Int)(body: Rand ?=> A)(using handler: Rand): A =
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
  def record[A](body: Rand ?=> A)(using handler: Rand): Recorded[A] =
    val state = List.newBuilder[Int]

    given Rand = (max: Int) =>
      val value = handler.nextInt(max)
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

  // - Basic generators ------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def int(max: Int)(using handler: Rand): Int = handler.nextInt(math.max(0, max))

  def choose(min: Int, max: Int)(using Rand): Int =
    int(max - min) + min

  def choose(min: Char, max: Char)(using Rand): Char =
    choose(min.toInt, max.toInt).toChar

  def choose(min: Short, max: Short)(using Rand): Short =
    choose(Short.MinValue.toInt, Short.MaxValue.toInt).toShort

  def boolean(using Rand): Boolean =
    int(2) == 1

  def size(using Rand, Size): Int = int(Size.size)

  def option[A](a: => A)(using Rand): Option[A] =
    frequency(
      1 -> None,
      9 -> Some(a)
    )

  def either[A, B](a: => A, b: => B)(using Rand): Either[A, B] =
    if boolean
    then Left(a)
    else Right(b)

  // - Character generators --------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def alphaLowerChar(using Rand): Char =
    choose('a', 'z')

  def alphaUpperChar(using Rand): Char =
    choose('A', 'Z')

  def alphaChar(using Rand): Char =
    oneOf(alphaLowerChar, alphaUpperChar)

  def numChar(using Rand): Char =
    choose('0', '9')

  def alphaNumChar(using Rand): Char =
    oneOf(alphaLowerChar, alphaUpperChar, numChar)

  def asciiChar(using Rand): Char =
    choose(0, 127).toChar

  def asciiPrintableChar(using Rand): Char =
    choose(32, 126).toChar

  def hexChar(using Rand): Char =
    val c = int(23)

    val code =
      if c < 10 then '0'.toInt + c.toInt
      else if c < 16 then c + 'a'.toInt - 10
      else c + 'A'.toInt - 16

    code.toChar

  // - Generator constructors ------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def oneOf[A](head: => A, tail: (Rand ?=> A)*)(using Rand): A =
    val content: Seq[Rand ?=> A] = head +: tail
    oneOf(content)

  // Note that `tail` is a vararg of context functions rather than by-names, because you apparently cannot take
  // by-names as varargs. Also, the capset variable is because of what I'm assuming is a bug that I hope to see
  // fixed: https://users.scala-lang.org/t/suspicious-reach-capability-error/12190
  def oneOf[A, B^](content: Seq[Rand ?->{B} A])(using Rand): A =
    if content.isEmpty then sys.error("Cannot pick one of a collection of 0 generators")
    else content(int(content.length))

  def frequency[A](head: (Int, Rand ?=> A), tail: (Int, Rand ?=> A)*)(using Rand): A =
    val content: Seq[(Int, Rand ?=> A)] = head +: tail
    frequency(content)

  def frequency[A](content: Seq[(Int, Rand ?=> A)])(using Rand): A =
    case class Frequencies(total: Int = 0, generators: List[(Int, Rand ?=> A)] = List.empty):
      def add(weight: Int, gen: Rand ?=> A): Frequencies =
        if weight <= 0 then this
        else Frequencies(total + weight, (weight, gen) :: generators)

      def pick: A =
        def loop(choice: Int, remaining: List[(Int, Rand ?=> A)]): A =
          // This awkward code is due to capture checking and how it apparently doesn't behave all that well
          // in pattern matches at the time of writing.
          val head: (Int, Rand ?=> A) = remaining.headOption
            .getOrElse(sys.error("frequency generated an impossible choice"))
          val (weight, value) = head

          val newChoice = choice - weight
          if newChoice < 0 then value
          else loop(newChoice, remaining.drop(1))

        loop(int(total), generators)

    val frequencies = content.foldLeft(Frequencies()):
      case (out, (weight, gen)) => out.add(weight, gen)

    frequencies.pick

  // - String generators -----------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** Generates a string of exactly `n` characters. */
  def stringOfN(n: Int, content: => Char): String =
    buildableOfN(n, content)

  /** Generates a string whose length is controlled by `Size`. */
  def stringOf(content: => Char)(using Rand, Size): String =
    stringOfN(size, content)

  /** Generates a non-empty-string whose length is controlled by `Size`. */
  def nonEmptyStringOf(content: => Char)(using Rand, Size): String =
    nonEmptyBuildableOf(content)

  def identifier(using Rand, Size): String =
    // Always generate the things with the most influence earlier, as it helps with shrinking.
    val tail = stringOfN(int(Size.size - 1), alphaNumChar)
    val head = alphaLowerChar

    s"$head$tail"

  def numString(using Rand, Size): String =
    stringOf(numChar)

  def alphaUpperString(using Rand, Size): String =
    stringOf(alphaUpperChar)

  def alphaLowerString(using Rand, Size): String =
    stringOf(alphaLowerChar)

  def alphaString(using Rand, Size): String =
    stringOf(alphaChar)

  def alphaNumString(using Rand, Size): String =
    stringOf(alphaNumChar)

  def asciiString(using Rand, Size): String =
    stringOf(asciiChar)

  def asciiPrintableString(using Rand, Size): String =
    stringOf(asciiPrintableChar)

  def hexString(using Rand, Size): String =
    stringOf(hexChar)

  // - Collections -----------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def buildableOfN[A, O](n: Int, content: => A)(using factory: Factory[A, O]): O =
    val builder = factory.newBuilder
    (0 to n).foreach: _ =>
      builder += content
    builder.result

  def buildableOf[A, O](content: => A)(using factory: Factory[A, O], r: Rand, s: Size): O =
    buildableOfN(size, content)

  def nonEmptyBuildableOf[A, O](content: => A)(using factory: Factory[A, O], r: Rand, s: Size): O =
    buildableOfN(math.max(1, size), content)

  def listOfN[A](n: Int, content: => A): List[A] =
    buildableOfN(n, content)

  def listOf[A](content: => A)(using Rand, Size): List[A] =
    listOfN(size, content)

  def nonEmptyListOf[A](content: => A)(using Rand, Size): List[A] =
    nonEmptyBuildableOf(content)

  def mapOfN[K, V](n: Int, content: => (K, V)): Map[K, V] =
    buildableOfN(n, content)

  def mapOf[K, V](content: => (K, V))(using Rand, Size): Map[K, V] =
    buildableOf(content)

  def nonEmptyMapOf[K, V](content: => (K, V))(using Rand, Size): Map[K, V] =
    nonEmptyBuildableOf(content)

  def infiniteList[A](content: => A): LazyList[A] =
    LazyList.continually(content)
