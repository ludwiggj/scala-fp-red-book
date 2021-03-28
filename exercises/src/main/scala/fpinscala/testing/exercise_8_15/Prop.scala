package fpinscala.testing.exercise_8_15

import fpinscala.laziness.Stream
import fpinscala.state.RNG
import fpinscala.testing.take2.Constants.{FailedCase, MaxSize, SuccessCount, TestCases}
import fpinscala.testing.take2.{Gen, SGen}
import fpinscala.testing.take2.Gen.randomStream
import fpinscala.testing.take2.SGen.listOfLAlternative

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

// Introduced for section 8.4.2, introduction of Proved case object
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

  val maxPropTrue = forAll(listOfLAlternative(smallInt)) { ns =>
    !ns.exists(_ > ns.max)
  }

  val maxPropFalse = forAll(listOfLAlternative(smallInt)) { ns =>
    !ns.exists(_ >= ns.max)
  }
}