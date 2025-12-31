package kantan.tests

import Prompt.*
import Assert.*

object ScalaCheck extends TestSuite:
  test("list tail"):
    val n = Params.param("n", Rand.int(100))
    val l = Params.param("l", Rand.list(Rand.int(100)))

    assertEquals((n :: l).tail, l)

  test("list head"):
    val l = Params.param("l", Rand.list(Rand.int(100)))

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
    val p = Params.param("p", genPerson)
    assertEquals(p.isTeenager, p.age >= 13 && p.age <= 19)
