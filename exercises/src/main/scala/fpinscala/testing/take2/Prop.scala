package fpinscala.testing.take2

import fpinscala.laziness.Stream
import fpinscala.state.{RNG, State}
import fpinscala.testing.take2.Constants.{FailedCase, MaxSize, SuccessCount, TestCases}
import fpinscala.testing.take2.Gen.{listOfN, randomStream, unit}
import fpinscala.testing.take2.SGen.listOf

object Constants {
  type TestCases = Int
  type FailedCase = String
  type SuccessCount = Int
  type MaxSize = Int
}

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

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  // Exercise 8.9
  def &&(p: Prop): Prop = Prop {
    (max, n, rng) =>
      val r1 = this.run(max, n, rng)
      val r2 = p.run(max, n, rng)

      (r1, r2) match {
        case (Passed, Passed) => Passed
        case (Falsified(_, _), _) => r1
        case _ => r2
      }
  }

  // Textbook answer runs them sequentially
  def &&&(p: Prop): Prop = Prop {
    (max, n, rng) =>
      tag("Left").run(max, n, rng) match {
        case Passed => p.tag("Right").run(max, n, rng)
        case x => x
      }
  }

  def ||(p: Prop): Prop = Prop {
    (max, n, rng) =>
      val r1 = this.run(max, n, rng)
      val r2 = p.run(max, n, rng)

      (r1, r2) match {
        case (Passed, _) => Passed
        case (_, Passed) => Passed
        case (Falsified(_, _), _) => r1
        case _ => r2
      }
  }

  // Textbook answer runs them sequentially
  def |||(p: Prop): Prop = Prop {
    (max, n, rng) =>
      tag("Left").run(max, n, rng) match {
        // In case of failure, run the other prop.
        case Falsified(_, _) => p.tag("Right").run(max, n, rng)
        case x => x
      }
  }

  // This is rather simplistic - in the event of failure, we simply prepend
  // the given message on a newline in front of the existing message.
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

  // Exercise 8.14 - Property to test List.sorted
  def sortedPrep[A](g: Gen[A])(implicit ord: Ordering[A]): Prop = {
    forAll(listOf(g)) { ns =>
      println(s"Candidate = $ns")
      ns.sorted.sliding(2, 1).toList.filter(_.size == 2).forall(l => ord.lteq(l(0), l(1)))
    }
  }

  def sortedPropTextbook[A](g: Gen[A])(implicit ord: Ordering[A]): Prop = {
    forAll(listOf(g)) { ns =>
      val nss = ns.sorted

      (nss.isEmpty || nss.tail.isEmpty || !nss.zip(nss.tail).exists {
        case (a, b) => ord.gt(a, b)
      }) &&
        ns.forall(nss.contains(_)) &&
        nss.forall(ns.contains(_))
    }
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
    }
    result
  }

  def check(p: => Boolean): Prop = {
    lazy val result = p
    forAll(unit(()))(_ => result)
  }

  def checkOnce(p: => Boolean): Prop = Prop {
    (_, _, _) => if (p) Passed else Falsified("()", 0)
  }
}

case class Gen[+A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))

  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    Gen(sample.flatMap(f(_).sample))
  }

  // Exercise 8.10
  def unsized: SGen[A] = {
    SGen(_ => this)
  }

  // Exercise 8.12
  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size flatMap (n => Gen.listOfN(n, this))

  // Required for refactoring (page 140)
  def map2[B, C](b: Gen[B])(f: (A, B) => C): Gen[C] = Gen {
    sample.map2(b.sample)(f)
  }

  // Introduce a combinator, again for refactoring (page 141)
  def **[B](g: Gen[B]): Gen[(A, B)] =
    (this map2 g)((_, _))
}

object Gen {
  // Generates an infinite stream of A values by repeatedly sampling a generator
  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  // Exercise 8.12
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequenceTextbook(List.fill(n)(g.sample)))

  def unit[A](a: => A): Gen[A] =
    Gen(State.unit(a))

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

  // Added for Parser combinator label law, see page 162
  private val chars = (' ' to '~').toList

  def map[A, B](g: Gen[A])(f: A => B): Gen[B] =
    Gen(g.sample.map(f))

  val charGen: Gen[Char] = map(choose(0, chars.size))(chars(_))

  def stringGen(length: Int): Gen[String] = map(listOfN(length, charGen))(_.mkString)
}

case class SGen[+A](g: Int => Gen[A]) {
  // Exercise 8.11
  def map[B](f: A => B): SGen[B] =
    g(0).map(f).unsized

  def mapTextbook[B](f: A => B): SGen[B] =
    SGen { i => {
      g(i) map f
    }
    }

  // Dropping the i parameter
  def mapTextbook2[B](f: A => B): SGen[B] =
    SGen {
      g(_) map f
    }

  def flatMap[B](f: A => SGen[B]): SGen[B] = {
    val g2: Int => Gen[B] = n => {
      g(n) flatMap { a => f(a).g(n) }
    }
    SGen(g2)
  }
}

object SGen {
  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => g.listOfN(unit(n)))

  def listOfAlternative[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => listOfN(n, g))

  // Exercise 8.13
  def listOfL[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => g.listOfN(unit(n + 1)))

  def listOfLAlternative[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => g.listOfN(unit(n max 1)))
}