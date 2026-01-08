package kantan.tests

import Prompt.*
import Assert.*

object ScalaCheck extends TestSuite:
  test("list tail"):
    val n = Rand.int(100).logAs("n")
    val l = Rand.list(Rand.int(100)).logAs("l")

    assertEquals((n :: l).tail, l)

  test("list head"):
    val l = Rand.list(Rand.int(100)).logAs("l")

    assertEquals(l.head, l(0))

  case class Person(
      firstName: String,
      lastName: String,
      age: Int
  ) {
    def isTeenager = age >= 13 && age <= 19
  }

  val genPerson: Rand ?-> Person =
    Person(
      Rand.oneOf("Alan", "Ada", "Alonzo"),
      Rand.oneOf("Lovelage", "Turning", "Church"),
      Rand.range(1, 100)
    )

  test("ex1"):
    val p = genPerson.logAs("p")
    assertEquals(p.isTeenager, p.age >= 13 && p.age <= 19)
