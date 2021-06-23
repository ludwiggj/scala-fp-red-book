package fpinscala.monads

import com.typesafe.scalalogging.LazyLogging
import fpinscala.UnitSpec
import fpinscala.monads.Monad.{Id, Reader, listMonad, optionMonad, stateMonad}
import fpinscala.state.RNG.{Simple, sequenceTextbookFoldLeft}
import fpinscala.state.{RNG, State}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class MonadSpec extends UnitSpec with ScalaCheckPropertyChecks with LazyLogging {

  "map2" can "map lists" in {
    val inputL: List[Int] = List(1, 2)
    val inputLL: List[List[Int]] = List(List(3, 4), List(5, 6))

    val result = listMonad.map2(inputL, inputLL)((a, b) => a :: b)
    assert(result == List(List(1, 3, 4), List(1, 5, 6), List(2, 3, 4), List(2, 5, 6)))
  }

  // For `List`, the `replicateM` function will generate a list of lists. It will contain all the lists of length `n`
  // with elements selected from the input list.

  "replicateM" can "replicate list once" in {
    val input = List(1, 2, 3)

    val expectedResult = List(List(1), List(2), List(3))

    val result = listMonad.replicateM[Int](1, input)
    assert(result == expectedResult)
  }

  it can "replicate list twice" in {
    val input = List(1, 2, 3)

    val expectedResult = List(
      List(1, 1), List(1, 2), List(1, 3),
      List(2, 1), List(2, 2), List(2, 3),
      List(3, 1), List(3, 2), List(3, 3)
    )

    val result = listMonad.replicateM[Int](2, input)
    assert(result == expectedResult)
  }

  it can "replicate list three times" in {
    val input = List(1, 2, 3)

    val expectedResult = List(
      List(1, 1, 1), List(1, 1, 2), List(1, 1, 3),
      List(1, 2, 1), List(1, 2, 2), List(1, 2, 3),
      List(1, 3, 1), List(1, 3, 2), List(1, 3, 3),

      List(2, 1, 1), List(2, 1, 2), List(2, 1, 3),
      List(2, 2, 1), List(2, 2, 2), List(2, 2, 3),
      List(2, 3, 1), List(2, 3, 2), List(2, 3, 3),

      List(3, 1, 1), List(3, 1, 2), List(3, 1, 3),
      List(3, 2, 1), List(3, 2, 2), List(3, 2, 3),
      List(3, 3, 1), List(3, 3, 2), List(3, 3, 3)
    )

    val result = listMonad.replicateM[Int](3, input)
    assert(result == expectedResult)
  }

  // For `Option`, it will generate either `Some` or `None` based on whether the input is `Some` or `None`.
  // The `Some` case will contain a list of length `n` that repeats the element in the input `Option`.

  it can "replicate Some option once" in {
    val input = Option(4)

    val expectedResult = Option(List(4))

    val result = optionMonad.replicateM[Int](1, input)
    assert(result == expectedResult)
  }

  it can "replicate Some option twice" in {
    val input = Option(4)

    val expectedResult = Option(List(4, 4))

    val result = optionMonad.replicateM[Int](2, input)
    assert(result == expectedResult)
  }

  it can "replicate Some option three times" in {
    val input = Option(4)

    val expectedResult = Option(List(4, 4, 4))

    val result = optionMonad.replicateM[Int](3, input)
    assert(result == expectedResult)
  }

  it can "replicate None option once" in {
    val input = None

    val expectedResult = None

    val result = optionMonad.replicateM[Int](1, input)
    assert(result == expectedResult)
  }

  it can "replicate None option twice" in {
    val input = None

    val expectedResult = None

    val result = optionMonad.replicateM[Int](2, input)
    assert(result == expectedResult)
  }

  it can "replicate None option three times" in {
    val input = None

    val expectedResult = None

    val result = optionMonad.replicateM[Int](3, input)
    assert(result == expectedResult)
  }

  "filterM" can "filter list on list criteria, single element" in {
    val input = List(1, 2, 3, 4, 5)

    val expectedResult = List(List(4, 5))

    val result = listMonad.filterM(input)(n => List(n > 3))
    assert(result == expectedResult)

    val resultTextbook = listMonad.filterMTextbook(input)(n => List(n > 3))
    assert(resultTextbook == expectedResult)
  }

  it can "filter list on list criteria, two elements" in {
    val input = List(1, 2, 3, 4, 5)

    val l45 = List(4, 5)
    val l4 = List(4)
    val l454 = List(l45, l4)
    val l454Times4 = l454 ++ l454 ++ l454 ++ l454

    val l245 = List(2, 4, 5)
    val l24 = List(2, 4)
    val l24524 = List(l245, l24)
    val l24524Times4 = l24524 ++ l24524 ++ l24524 ++ l24524

    val expectedResult = l454Times4 ++ l24524Times4 ++ l454Times4 ++ l24524Times4

    val result = listMonad.filterM(input)(n => List(n > 3, n % 2 == 0))
    assert(result == expectedResult)

    val resultTextbook = listMonad.filterMTextbook(input)(n => List(n > 3, n % 2 == 0))
    assert(resultTextbook == expectedResult)
  }

  it can "filter list on option criteria, Some" in {
    val input = List(1, 2, 3, 4, 5)

    val expectedResult = Some(List(4, 5))

    val result = optionMonad.filterM(input)(n => Some(n > 3))
    assert(result == expectedResult)

    val resultTextbook = optionMonad.filterMTextbook(input)(n => Some(n > 3))
    assert(resultTextbook == expectedResult)
  }

  it can "filter list on option criteria, None" in {
    val input = List(1, 2, 3, 4, 5)

    val expectedResult = None

    val result = optionMonad.filterM(input)(_ => None)
    assert(result == expectedResult)

    val resultTextbook = optionMonad.filterMTextbook(input)(_ => None)
    assert(resultTextbook == expectedResult)
  }

  def firstComposeIdentityLaw(f: Int => Option[String]): Unit = {
    val input = 6
    val result1: Option[String] = optionMonad.compose(f, optionMonad.unitEager[String])(input)
    val result2: Option[String] = f(input)

    assert(result1 == result2)
  }

  // Exercise 11.11 - prove identity laws hold for monad of my choice - Option!
  "first compose identity law" should "hold for option (Some)" in {
    firstComposeIdentityLaw(i => Some(i.toString))
  }

  it should "hold for option (None)" in {
    firstComposeIdentityLaw(_ => None)
  }

  def secondComposeIdentityLaw(f: Int => Option[String]): Unit = {
    val input = 6
    val result1: Option[String] = optionMonad.compose(optionMonad.unitEager[Int], f)(input)
    val result2: Option[String] = f(input)

    assert(result1 == result2)
  }

  "second compose identity law" should "hold for option (Some)" in {
    secondComposeIdentityLaw(i => Some(i.toString))
  }

  it should "hold for option (None)" in {
    secondComposeIdentityLaw(_ => None)
  }

  def firstFlatmapIdentityLaw(x: Option[Int]): Unit = {
    val unit = optionMonad.unitEager[Int] _

    assert(optionMonad.flatMap(x)(unit) == x)
  }

  "first flatMap identity law" should "hold for option (Some)" in {
    firstFlatmapIdentityLaw(Some(6))
  }

  it should "hold for option (None)" in {
    firstFlatmapIdentityLaw(None)
  }

  def secondFlatmapIdentityLaw(x: Option[Int]): Unit = {
    val unit = optionMonad.unitEager[Option[Int]] _
    val f: Option[Int] => Option[String] = i => i.map(_.toString)

    assert(optionMonad.flatMap(unit(x))(f) == f(x))
  }

  "second flatMap identity law" should "hold for option (Some)" in {
    secondFlatmapIdentityLaw(Some(7))
  }

  it should "hold for option (None)" in {
    secondFlatmapIdentityLaw(None)
  }

  "id" should "flatmap" in {
    val expectedResult = Id("Hello, monad!")

    val result = Id("Hello, ") flatMap (a =>
      Id("monad!") flatMap (b =>
        Id(a + b)
        )
      )

    assert(result == expectedResult)

    val result2 = for {
      a <- Id("Hello, ")
      b <- Id("monad!")
    } yield a + b

    assert(result2 == expectedResult)
  }

  // Exercise 11.18

  // Replicates state, and runs them in sequence

  // Textbook: `replicateM` for `State` repeats the same state transition a number of times and returns a list of the
  // results. It's not passing the same starting state many times, but chaining the calls together so that the output
  // state of one is the input state of the next.
  it should "replicateM" in {
    val stringStateMonad = stateMonad[String]
    val expectedList = List(4, 4, 4)

    assert(stringStateMonad.replicateM(3, State.unit(4)).run("go")._1 == expectedList)
  }


  // map2 lifts function to operate in context of State

  // Textbook: `map2` works similarly in that it takes two state transitions and feeds the output state of one to the
  // input of the other. The outputs are not put in a list, but combined with a function `f`.
  "state monad" should "map2 maps two states together" in {
    val stringStateMonad = stateMonad[String]
    assert(stringStateMonad.map2(stringStateMonad.unit(4), stringStateMonad.unit(3))(_ * _).run("Hello")._1 == 12)
  }

  // Sequences state computations to run one after the other

  // Textbook: `sequence` takes an entire list of state transitions and does the same kind of thing as `replicateM`:
  // it feeds the output state of the first state transition to the input state of the next, and so on.
  // The results are accumulated in a list.
  it should "sequence computations" in {
    val stringStateMonad = stateMonad[String]

    val inputList: List[State[String, Int]] = List(State.unit(1), State.unit(2), State.unit(3))
    val expectedList = List(1, 2, 3)

    assert(stringStateMonad.sequence(inputList).run("go")._1 == expectedList)
  }

  it should "number list elements via for comprehension" in {
    val F = stateMonad[Int]

    def zipWithIndex[A](as: List[A]): List[(Int, A)] = {
      val accStart: State[Int, List[(Int, A)]] = F.unit(List[(Int, A)]())

      val v: State[Int, List[(Int, A)]] = as.foldLeft(accStart)((acc, a) => for {
        xs <- acc
        n <- State.get        // Returns state that exposes state in result output
        _ <- State.set(n + 1) // Returns state with its result state set. It disregards input and exposes no output in result
      } yield (n, a) :: xs)

      v.run(0)._1.reverse
    }

    assert(zipWithIndex(List("Five", "Four", "Three", "Two", "One", "Zero")) ==
      List((0, "Five"), (1, "Four"), (2, "Three"), (3, "Two"), (4, "One"), (5, "Zero"))
    )
  }

  it should "number list elements via flatMap and map" in {
    val F = stateMonad[Int]

    def zipWithIndex[A](as: List[A]): List[(Int, A)] = {
      val accStart: State[Int, List[(Int, A)]] = F.unit(List[(Int, A)]())

      // case class State[S, +A](run: S => (A, S))
      // def flatMap[B](f: A => State[S, B]): State[S, B]
      // def get[S]: State[S, S]
      // def map[B](f: A => B): State[S, B]
      val v: State[Int, List[(Int, A)]] = as.foldLeft(accStart)((stateAcc, a) =>
        stateAcc.flatMap(xs => State.get.flatMap(
          nInt => State.set(nInt + 1).map(
            _ => (nInt, a) :: xs
          )
        ))
      )

      v.run(0)._1.reverse
    }

    assert(zipWithIndex(List("Five", "Four", "Three", "Two", "One", "Zero")) ==
      List((0, "Five"), (1, "Four"), (2, "Three"), (3, "Two"), (4, "One"), (5, "Zero"))
    )
  }

  "reader monad" should "return argument from unit when run" in {
    val stringReader = Monad.readerMonad[String]

    assert(stringReader.unit(1).run("one") == 1)
  }

  // flatMap: Reads input to produce A, then transformed to a B via supplied function f

  // Textbook: The action of Reader's `flatMap` is to pass the `r` argument along to both
  // the  outer Reader and also to the result of `f`, the inner Reader. Similar to how
  // `State` passes along a state, except that in `Reader` the "state" is read-only.
  it should "combine via flatMap" in {

    val stringReader = Monad.readerMonad[String]

    assert(stringReader.flatMap(stringReader.unit(1))(a => stringReader.unit("X" * (a * 2)))
      .run("one") == "XX"
    )
  }

  // Sequence takes a list of readers, returns a single reader that will transform input via
  // each of readers in turn returning all results in a list (N.B. No chaining, since type
  // signature is Reader[R, A])

  // Textbook: The meaning of `sequence` here is that if you have a list of functions, you can
  // turn it into a function that takes one argument and passes it to all the functions
  // in the list, returning a list of the results.
  it should "sequence readers" in {
    val stringReader = Monad.readerMonad[Int]

    val readerList = List[Reader[Int, Int]](
      Reader(_ * 2),
      Reader(_ + 5),
      Reader(_ * 4),
      Reader(_ - 3),
      Reader(_ / 5)
    )

    val expectedResult = List(10, 10, 20, 2, 1)

    val sequencer = stringReader.sequence(readerList)

    assert(sequencer.run(5) == expectedResult)
  }

  // Join discards the outer reader

  // Textbook: The meaning of `join` is simply to pass the same value as both arguments to a
  // binary function.
  it should "join readers" in {
    val stringReader = Monad.readerMonad[Int]

    val singleReader: Reader[Int, String] = stringReader.unit("2")
    val doubleReader: Reader[Int, Reader[Int, String]] = stringReader.unit(singleReader)

    assert(stringReader.join(doubleReader).run(5) == "2")
  }

  // replicateM replicates a single reader that will transform input via each of readers in turn
  // returning all results in a list (N.B. No chaining, since type signature is Reader[R, A])
  // Hence all results will be the same

  // Textbook: The meaning of `replicateM` is to apply the same function a number of times to
  // the same argument, returning a list of the results. Note that if this function is _pure_,
  // (which it should be), this can be exploited by only applying the function once and
  // replicating the result instead of calling the function many times. This means the Reader
  // monad can override replicateM to provide a very efficient implementation.
  it should "replicate readers" in {
    val stringReader = Monad.readerMonad[Int]

    val readers = stringReader.replicateM(3, Reader(_ * 3))

    assert(readers.run(5) == List(15, 15, 15))
  }

  // Monad laws...

  // identity laws
  // -------------

  // (1) flatMap(x)(unit) == x
  // Reading input, return result, then flatmapping it with another reader that just returns
  // same value, is same as result returned by orighinal reader

  // (2) flatMap(unit(y))(f) == f(y)
  // Flatmapping a reader that returns y with function f is same as applying function f
  // directly to y

  // associative law
  // ---------------

  // (1) join(join(x)) == join(map(x)(join))

  // Join is reading input to produce A, then return it

  // For `Parser`, the `join` combinator is running the outer parser to produce a `Parser`, then running the inner
  // `Parser` _on the remaining input_. The associative law is saying, roughly, that only the _order_ of nesting
  // matters, since that's what affects the order in which the parsers are run.

  // TODO So is it equivalent for the reader monad as per the parser (described above)?
}







