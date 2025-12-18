# kantan.tests

## Origin of the project

This project is kind of a reaction to the pushback I get whenever I mention Capabilities as an interesting alternative to monads for effect encoding. More often than not, I'm told that it's all well and good for toy programs, but it wouldn't work with "real programming" - always a slightly insulting argument, the subtext clearly being that what I just showed isn't _real programming_.

I tend to not react very maturely to feeling insulted, which is exactly what happened here: kantan.tests is me taking something that has been implemented quite a lot with monads (property based testing), and rewriting it entirely with capabilities in a direct style, out of spite.

As is sometimes the case, what I start doing out of pettiness ends up being quite interesting after all. I'm not arguing that anyone should use kantan.tests, at least not in its current state, but it's fun! I may turn it into a full fledged, maintained project if there is interest, but at the moment, I think it serves as a practical demonstration that capabilities have legs.

## Some design notes

I may have gone slightly overboard with capabilities here. Just about everything I could model that way, I did. While I quite like where that lead me, maybe moderation would have been a better guiding principle.

I'm also using this project to explore the design space, and so some idioms I've come up with might be bad, or not what the community will eventually agree on. This is (currently) very much a research project, and some of the things I found might have been the wrong thing to find.

## Key features

kantan.csv has a set of features which I believe make it much friendlier _to beginners_ than the PBT libraries I'm used to working with. The learning curve is almost non-existent - if you know how to write example-based tests, you merely need to learn a couple of keywords (and not particularly hard ones at that - `test`, for example, feels manageable).

Of course, the tradeoff is that you can't always take things as far as you could with, say, ScalaCheck. The main (only?) example of that is test case reduction, which is much harder to get right with ScalaCheck, but once you've mastered the knack, allows you to tweak its behaviour in a way that kantan.tests simply doesn't and probably cannot.

### Unifies exampe-based and generative tests

Both kinds of tests are written exactly the same way, the only distinction is that generative tests call randomised functions. This makes it very comfortable to start from example-based tests and evolve them into generative ones, a workflow I'm quite fond of.

For example, testing a hypothetical sort function with examples:
``` scala
test("sort([4, 3, 2, 1]) = [1, 2, 3, 4]"):
  val input    = List(4, 3, 2, 1)
  val observed = sort(input)
  val expected = List(1, 2, 3, 4)

  assertEquals(expected, observed)
```

We can turn this in a generative test simply by changing the way the input is computed:
``` scala
test("forall input, sort(input) == input.sorted"):
  val input    = Rand.list(Rand.int(100))
  val observed = sort(input)
  val expected = input.sorted

  assertEquals(expected, observed)
```

This is achieved by having the body of a test depend on the `Rand` capability - it doesn't have to _use_ it, in which case you end up with an example-based test. Another lovely side effect is that you no longer need one test definition method per number of inputs the test takes, like ScalaCheck is forced to do - `test` covers every possible number of test inputs.

### Test case reduction for free

kantan.tests uses _internal_ test case reduction, meaning that developers need not do any work to get reduced test cases (indeed, they _cannot_, which one could argue is a drawback).

This means you never get massive failing test cases that a human can't reasonably hope to parse - you know, the ones full of long strings of random unicode characters, not all of them printing? Those ones. kantan.tests will always reduce them to something more manageable (although this needs further research - the result is much better than not reducing test cases, but sometimes inferior to what other libraries can achieve).

For example, the following (silly) test tries to prove that all strings have a odd length:

``` scala
test("Strings have odd lengths (this should fail)"):
  val input = Params.param("Input", Rand.identifier)
  
  Assert.assert(input.length % 2 != 0, s"'$input's length was even")  
```

This fails almost immediately with the following report:

```
* Strings have odd lengths (this should fail) (6 successful attempt(s))
  Error: 'aa's length was even
  Parameters (shrunk 10 time(s)):
    - Input = aa
  Seed:  -669210893537017304
  Size:  6
```

We're getting a very reasonable failing test case: `aa`, and no particular work was done in the test or its generators to achieve this result.


### Valid reduced test cases

One problem with tools such as ScalaCheck is how they decorelate test case generation and reduction. This often results in reduced test cases being invalid: your test might, for example, take a non-empty string for input, which is enforced by the generator. The reducer is not aware of this constraint and will happily reduce a "valid" failing test case to the empty string, which is both confusing and not very useful.

The way kantan.tests works makes it impossible to reduce to invalid test cases - under the hood, we reduce _the randomizer_ and re-run test case generation, so any reduced test case is, by definition, something that could have been produced by the generator.

And yes, reducing the randomizer is a little weird and crazy and very much something I need to put more research in.


### Natural writing style

kantan.tests lets you write your tests in a direct style, which means no monads, traverse... if you need a random value, you just get it and keep working as usual. The result is, I feel, easier to read and write, and entirely more natural.

### Compared with ScalaCheck

Here's a property taken directly from the ScalaCheck examples (and cleaned up a little for Scala 3 syntax):
``` scala
val genPerson =
  for
    firstName <- oneOf("Alan", "Ada", "Alonzo")
    lastName <- oneOf("Lovelace", "Turing", "Church")
    age <- choose(1, 100)
  yield Person(firstName, lastName, age)

implicit val arbPerson: Arbitrary[Person] = Arbitrary(genPerson)

property("ex1") = Prop.forAll: (p: Person) =>
  p.isTeenager == (p.age >= 13 && p.age <= 19)
```

The kantan.tests version of the same test is:
``` scala
test("ex1"):
  val p = Params.param("p", Person(
    firstName = Rand.oneOf("Alan", "Ada", "Alonzo"),
    lastName  = Rand.oneOf("Lovelace", "Turing", "Church"),
    age       = Rand.range(1, 100)
  ))

  assertEquals(p.isTeenager, p.age >= 13 && p.age <= 19)
```

### Compared with HedgeHog
Here's a property taken directly from the HedgeHog examples (and cleaned up a little for Scala 3 syntax):

``` scala
def example1: Property =
  for
    x <- Gen.char('a', 'z').log("x")
    y <- int(Range.linear(0, 50)).lift
  yield Result.assert(y < 87 && x <= 'r')
```

The kantan.tests version of the same test is:
``` scala
test("example1"):
  val x = Params.param("x", Rand.lowerAscii)
  val y = Params.param("y", Rand.int(50))

  Assert.assert(y < 87 && x <= 'r')
```
  
