package kantan.tests

import caps.unsafe.unsafeAssumePure
import collection.mutable.Builder

// ---------------------------------------------------------------------------------------------------------------------
//
// Groups tests together for ease of execution.
//
// ---------------------------------------------------------------------------------------------------------------------

private case class Test(desc: String, body: Configuration => TestOutcome)

/** Set of tests, conventionally related to the same feature.
  *
  * The body of a `TestSuite` always has a `Runner` available, which allows implementers to write things like:
  * {{{
  *  class MyTest extends TestSuite:
  *    test("The foo should Bar"):
  *      assert(Rand.int(100) < 100, "Woops")
  * }}}
  */
trait TestSuite:
  private val testBuilder: Builder[Test, List[Test]] = List.newBuilder

  given Runner:
    override def run(name: String, body: Configuration => TestOutcome) =
      testBuilder += Test(name, body).unsafeAssumePure

  val run: Runner ?-> Unit = testBuilder.result.foreach: test =>
    Runner.run(test.desc):
      test.body(Conf.get)
