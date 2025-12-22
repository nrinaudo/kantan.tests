package kantan.tests

@main def run =
  // val suites = List(ListSort, BadTest, Hedgehog, ScalaCheck)
  val suites = List(ShrinkOptim)

  given Runner = new ConsoleRunner(Conf(100, 0, 100))

  suites.foreach(_.run)
