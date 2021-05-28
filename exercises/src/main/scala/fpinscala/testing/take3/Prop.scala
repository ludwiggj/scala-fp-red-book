package fpinscala.testing.take3

import fpinscala.laziness.Stream
import fpinscala.parallelism.blocking.Par
import fpinscala.parallelism.blocking.Par._
import fpinscala.state.{RNG, State}
import fpinscala.testing.take2.Constants.{FailedCase, MaxSize, SuccessCount, TestCases}
import fpinscala.testing.take2.{Gen, SGen}
import fpinscala.testing.take2.Gen.{choose, randomStream}
import fpinscala.testing.take2.SGen.listOfLAlternative

import java.util.concurrent.{ExecutorService, Executors}

sealed trait Result {
  def isFalsified: Boolean
}

case object Passed extends Result {
  override def isFalsified: Boolean = false
}

case class Falsified(failure: FailedCase,
                     successes: SuccessCount) extends Result {
  override def isFalsified: Boolean = true
}

case object Proved extends Result {
  override def isFalsified: Boolean = false
}

// NOTE: Introduced for section 8.4.2, introduction of Proved case object
case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = Prop {
    (max, n, rng) =>
      tag("Left").run(max, n, rng) match {
        case Passed | Proved => p.tag("Right").run(max, n, rng)
        case x => x
      }
  }

  def ||(p: Prop): Prop = Prop {
    (max, n, rng) =>
      tag("Left").run(max, n, rng) match {
        // In case of failure, run the other prop.
        case Falsified(_, _) => p.tag("Right").run(max, n, rng)
        case x => x
      }
  }

  def tag(msg: String): Prop = Prop {
    (max, n, rng) =>
      run(max, n, rng) match {
        case Falsified(e, c) => Falsified(msg + "\n" + e, c)
        case x => x
      }
  }
}

object Prop {

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = Prop {
    (_, n, rng) =>
      randomStream(gen)(rng).zipWith(Stream.from(0))((_, _)).take(n).map {
        // a stream of pairs (a, i) where a is a random value and i is its index in the stream
        case (a, i) => try {
          // When a test fails, record the failed case and its index so we know how many tests succeeded before it
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch {
          // If a test case generates an exception, record it in the result
          case e: Exception => Falsified(buildMsg(a, e), i)
        }
      }.find(_.isFalsified).getOrElse(Passed)
  }

  def buildMsg[A](s: A, e: Exception): String =
    s"test case $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  // Now create a forAll method for SGen
  def forAll[A](sgen: SGen[A])(f: A => Boolean): Prop = {
    forAll(i => sgen.g(i))(f)
  }

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      // For each size, generate this many random cases
      val casesPerSize = (n + (max - 1)) / max

      // make one property per size, but no more than n properties
      // (n min max) is there because if max size of input to test case is smaller than the desired number
      // of test cases to run, you can only run tests for up to max input size
      val props: Stream[Prop] = Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))

      // Combine stream of props into a single property
      val prop: Prop = props.map(
        p => Prop {
          (max, _, rng) =>
            p.run(max, casesPerSize, rng)
        }
      ).toList.reduce(_ && _)
      prop.run(max, n, rng)
  }

  def run(p: Prop,
          maxSize: MaxSize = 100,
          testCases: TestCases = 100,
          rng: RNG = RNG.Simple(System.currentTimeMillis())
         ): Result = {
    val result = p.run(maxSize, testCases, rng)
    result match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
        println(s"+ OK, proved property.")
    }
    result
  }

  def check(p: => Boolean): Prop = Prop {
    (_, _, _) => if (p) Proved else Falsified("()", 0)
  }

  val smallInt: Gen[Int] = Gen.choose(-10, 10)

  val maxPropTrue: Prop = forAll(listOfLAlternative(smallInt)) { ns =>
    !ns.exists(_ > ns.max)
  }

  val maxPropFalse: Prop = forAll(listOfLAlternative(smallInt)) { ns =>
    !ns.exists(_ >= ns.max)
  }

  // Property to check:
  // Par.map(Par.unit(1))(_ + 1) == Par.unit(2)

  val ES: ExecutorService = Executors.newCachedThreadPool()

  val parProp: Prop = Prop.forAll(Gen.unit(Par.unit(1)))(i =>
    Par.map(i)(_ + 1)(ES).get == Par.unit(2)(ES).get
  )

  val parProp2: Prop = Prop.check {
    val p = Par.map(Par.unit(1))(_ + 1)
    val p2 = Par.unit(2)
    p(ES).get == p2(ES).get
  }

  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] = {
    Par.map2(p, p2)(_ == _)
  }

  val parProp3: Prop = Prop.check {
    equal(
      Par.map(Par.unit(1))(_ + 1),
      Par.unit(2)
    )(ES).get
  }

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)

    Gen(State(RNG.double).flatMap(d =>
      if (d < g1Threshold) g1._1.sample else g2._1.sample
    ))
  }

  def weightedExecutorServiceGen(start: Int, stopExclusive: Int): Gen[ExecutorService] = weighted(
    choose(10, 40).map(Executors.newFixedThreadPool) -> .75,
    Gen.unit(Executors.newCachedThreadPool) -> .25
  )

  val S: Gen[ExecutorService] = weightedExecutorServiceGen(1, 4)

  def forAllParTake1[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S.map2(g)((_, _))) { case (s, a) => f(a)(s).get() }

  def forAllParTake2[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S ** g) { case (s, a) => f(a)(s).get() }

  // Custom extractor, http://mng.bz/4pUc)
  object ** {
    def unapply[A, B](p: (A, B)): Some[(A, B)] = Some(p)
  }

  def forAllPar[A](gES: Gen[ExecutorService])(ga: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(gES ** ga) { case s ** a => f(a)(s).get() }

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAllPar(S)(g)(f)

  def checkPar(p: => Par[Boolean]): Prop =
    forAllPar(Gen.unit(()))(_ => p)

  // TODO - This is proposed in book as a simplification, but as it stands it
  //        has changed a passing test from Proved to Passed
  val parProp4: Prop = checkPar {
    equal(
      Par.map(Par.unit(1))(_ + 1),
      Par.unit(2)
    )
  }

  // Another law for Par (page 141):

  // map(unit(x))(f) == unit(f(x))

  // which simplifies to:

  // map(y)(x => x) == y

  // Can we test this? Property states that equality holds for all choices of y, for all types.
  // We're forced to pick particular values for y:

  val genParInt: Gen[Par[Int]] = Gen.choose(0, 10).map(Par.unit)

  def parLaw[A](ga: Gen[Par[A]]): Prop =
    forAllPar(ga)(n => equal(Par.map(n) { y => y }, n))

  val parMapLaw1: Prop = parLaw(genParInt)

  // Exercise 8.16
  // A richer generator for Par[Int], which builds more deeply nested parallel computations

  // A `Gen[Par[Int]]` generated from a list summation that spawns a new parallel computation for each element
  // of the input list summed to produce the final result. This is not the most compelling example, but it
  // provides at least some variation in structure to use for testing.
  val genParInt2: Gen[Par[Int]] = choose(-100, 100).listOfN(choose(0, 5)).map(l =>
    l.foldLeft(Par.unit(0))((p, i) =>
      // Need to make sure we have enough threads to handle the forks
      Par.fork {
        Par.map2(p, Par.unit(i))(_ + _)
      }))

  def parLaw[A](gES: Gen[ExecutorService], ga: Gen[Par[A]]): Prop =
    forAllPar(gES)(ga)(n => equal(Par.map(n) { y => y }, n))

  val parMapLaw2: Prop = parLaw(weightedExecutorServiceGen(start = 10, stopExclusive = 15), genParInt2)

  // Exercise 8.17
  def forkLaw[A](ga: Gen[Par[A]]): Prop =
    forAllPar(ga)(n => equal(Par.fork(n), n))

  def forkLaw[A](gES: Gen[ExecutorService], ga: Gen[Par[A]]): Prop =
    forAllPar(gES)(ga)(n => equal(Par.fork(n), n))

  val parForkLaw1: Prop = forkLaw(genParInt)

  val parForkLaw2: Prop = forkLaw(weightedExecutorServiceGen(start = 10, stopExclusive = 15), genParInt2)

  // Exercise 8.18

  // takeWhile properties

  // Length should be between 0 and length of list
  // Length is 0 if no elements satisfy predicate
  // Length is n if all elements satisfy predicate
  // If length of result < input, element at (result.length + 1) fails the predicate

  // takeWhile and dropWhile

  // l.takeWhile(p) ++ l.dropWhile(p) = l

  // e.g.

  // (1, 2, 3, 4, 5)

  // takeWhile(x < 3) ++ dropWhile(x < 3) = (1, 2, 3, 4, 5)
  //    (1, 2)        ++    (3, 4, 5)     = (1, 2, 3, 4, 5)

  // takeWhile(!(x < 3)) ++ dropWhile(!(x < 3)) = (1, 2, 3, 4, 5)
  //         ()          ++    (1, 2, 3, 4, 5)  = (1, 2, 3, 4, 5)

  // takeWhile(even) ++ dropWhile(even) = (1, 2, 3, 4, 5)
  //       ()        ++ (1, 2, 3, 4, 5) = (1, 2, 3, 4, 5)

  // Answer (I effectively covered this):

  // `l.takeWhile(f) ++ l.dropWhile(f) == l`

  // We want to enforce that `takeWhile` returns the _longest_ prefix whose elements satisfy the predicate.
  // There are various ways to state this, but the general idea is that the remaining list, if non-empty,
  // should start with an element that does _not_ satisfy the predicate.

  // A property for takeWhile (page 143):

  private val isEven = (i: Int) => i % 2 == 0

  val takeWhileProp: Prop = Prop.forAll(Gen.listOfN(5, smallInt))(ns =>
    ns.takeWhile(isEven).forall(isEven)
  )

  def genStringIntFn(g: Gen[Int]): Gen[String => Int] =
    g map (i => _ => i)

  // Exercise 8.19

  // Generate a function that uses its argument in some way to select which int (boolean?) to return

  def genBooleanFn[A](choices: A => Boolean): Gen[A => Boolean] =
    Gen.unit(()).map(_ => a => choices(a))

  // See https://stackoverflow.com/questions/35390307/comparing-values-of-a-generic-type-in-generic-functions-in-scala
  def smallerThanGen[A](a: Gen[A])(implicit ev: A => Ordered[A]): Gen[A => Boolean] = {
    a.map(threshold => aa => aa < threshold)
  }

  type OrderedView[T] = T => Ordered[T]
  def smallerThanGen2[A : OrderedView](a: Gen[A]): Gen[A => Boolean] = {
    a.map(threshold => aa => aa < threshold)
  }

  private val smallerThan: Gen[Int => Boolean] = smallerThanGen(smallInt)

  val takeWhileProp2: Prop = Prop.forAll(Gen.listOfN(5, smallInt) ** smallerThan) { case (ns, fn) =>
    ns.takeWhile(fn).forall(fn)
  }

  def largerThanGen[A : OrderedView](a: Gen[A]): Gen[A => Boolean] = {
    a.map(threshold => aa => aa > threshold)
  }

  def stringGen(length: Int): Gen[String] = {
    val chars = (' ' to '~').toList
    val charGen: Gen[Char] = choose(0, chars.size).map(chars(_))

    Gen.listOfN(length, charGen).map(_.mkString)
  }

  private val largerThan: Gen[String => Boolean] = largerThanGen(stringGen(5))

  val takeWhileProp3: Prop = Prop.forAll(Gen.listOfN(5, stringGen(5)) ** largerThan) { case (ns, fn) =>
    ns.takeWhile(fn).forall(fn)
  }

  // Textbook Answer:
  // If we are just looking at the random case, one way to have the generated `Int` depend on the `String`
  // might be to set the seed of a new random number generator to be equal to the `hashCode` of the given
  // input `String`.

  // Let's start by looking at the signature of our motivating example, generating a function from
  // `String => Int` given a `Gen[Int]`:

  def genStringInt(g: Gen[Int]): Gen[String => Int] = ???

  // Let's generalize this a bit to not be specialized to `Int`, because that would let us cheat a bit
  // (by, say, returning the `hashCode` of the input `String`, which just so happens to be an `Int`).

  def genStringFn1[A](g: Gen[A]): Gen[String => A] = ???

  //  We want to use information from the input `String` to influence what `A` we generate.
  //  The only way we can have any influence on what value a `Gen` produces is to modify
  //  the `RNG` value it receives as input:

  def genStringFn2[A](g: Gen[A]): Gen[String => A] = Gen {
    State { (rng: RNG) => ??? }
  }

  // Where `???` has to be of type `(String => A, RNG)`, and moreover, we want the `String` to somehow
  // affect what `A` is generated. We do that by modifying the seed of the `RNG` before passing it to
  // the `Gen[A]` sample function. A simple way of doing this is to compute the hash of the input
  // string, and mix this into the `RNG` state before using it to produce an `A`:

  def genStringFn[A](g: Gen[A]): Gen[String => A] = Gen {
    State { (rng: RNG) =>
      // we still use `rng` to produce a seed, so we get a new function each time
      val (seed, rng2): (Int, RNG) = rng.nextInt
      val f = (s: String) => {
        // Declare a new RNG, dependent on the string
        val simple: RNG = RNG.Simple(seed.toLong ^ s.hashCode.toLong)
        g.sample.run(simple)._1  // Note that this RNG is discarded
      }
      (f, rng2)
    }
  }

  // More generally, any function which takes a `String` and an `RNG` and produces a new `RNG` could
  // be used. In above example we're computing the `hashCode` of the `String` and then XOR'ing it
  // with a seed value to produce a new `RNG`. We could just as easily take the length of the `String`
  // and use this value to perturb our RNG state, or take the first 3 characters of the string. The
  // choice affects what sort of function we are producing:

  // - If we use `hashCode` to perturb the `RNG` state, the function we are generating uses all the
  //   information of the `String` to influence the `A` value generated. Only input strings that
  //   share the same `hashCode` are guaranteed to produce the same `A`.

  // - If we use the `length`, the function we are generating is using only some of the information
  //   of the `String` to influence the `A` being generated. For all input strings that have the
  //   same length, we are guaranteed to get the same `A`.

  // The strategy we pick depends on what functions we think are realistic for our tests. Do we want
  // functions that use all available information to produce a result, or are we more interested in
  // functions that use only bits and pieces of their input? We can wrap the policy up in a `trait`:

  trait Cogen[-A] {
    def sample(a: A, rng: RNG): RNG
  }

  // As an exercise, try implementing a generalized version of `genStringFn`.
  // You can pattern the implementation after `genStringFn`. Just follow the types!

  def fn[A, B](in: Cogen[A])(out: Gen[B]): Gen[A => B] = Gen {
    State { (rng: RNG) =>
      val (_, rng2) = rng.nextInt
      val f = (a: A) => out.sample.run(in.sample(a, rng))._1
      (f, rng2)
    }
  }

  object HashCodeStrategy extends Cogen[String] {
    override def sample(a: String, rng: RNG): RNG = {
      RNG.Simple(a.hashCode.toLong)
    }
  }

  def fn1[B](out: Gen[B]): Gen[String => B] = fn(HashCodeStrategy)(out)

  object HashCodeStrategy2 extends Cogen[String] {
    override def sample(a: String, rng: RNG): RNG = {
      val (seed, _) = rng.nextInt
      RNG.Simple(seed.toLong ^ a.hashCode.toLong)
    }
  }

  // TODO -
  //  Exercise 8.15 - look at textbook example
  //  Exercise 8.19 - use genStringFn / fn1 to generate Gen[String => Boolean] to create a take while property?
  //  Exercise 8.20
}