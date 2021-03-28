package fpinscala.testing.take1

import fpinscala.state.State
import fpinscala.state.RNG
import fpinscala.state.RNG.Simple
import fpinscala.testing.take1.Gen._

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

// Exercise 8.1

// Properties for sum

// (1) Reversing a list and summing it should give same result as summing the original
// (2) Sum of all elements if they are all the same value = list length * element value
// (3) Sum of empty list is 0
// (4) Sum of list if all elements are 0 is 0
// (5) Sum of double of all elements = double sum of original list
// (6) Sum of elements and their opposites is 0

// When thinking of properties a function should satisfy, it often helps to consider
// inputs that have some structure that is easy to describe. A list where all the
// elements are the same is one simple structure.

// Answers. Here are a few properties:
//
// (1) The sum of the empty list is 0.
// (2) The sum of a list whose elements are all equal to `x` is just the list's length multiplied by `x`.
//     We might express this as `sum(List.fill(n)(x)) == n*x`
// (3) For any list, `l`, `sum(l) == sum(l.reverse)`, since addition is commutative.
// (4) Given a list, `List(x,y,z,p,q)`, `sum(List(x,y,z,p,q)) == sum(List(x,y)) + sum(List(z,p,q))`, since
//     addition is associative. More generally, we can partition a list into two subsequences whose sum is
//     equal to the sum of the overall list.
// (5) The sum of 1,2,3...n is `n*(n+1)/2`.

//Exercise 8.2

// Properties for sum that finds a maximum of a list of ints

// (1) Max of empty list is undefined
// (2) Max of single element list is the single element
// (3) Max of all elements if they are all the same value is that value
// (4) Max of list and reversed list is the same
// (5) Given a list, `List(x,y,z,p,q)`, `max(List(x,y,z,p,q)) == max(max(List(x,y)), max(List(z,p,q)))`

// Answers. Here are a few properties:
//
// (1) The max of a single element list is equal to that element.
// (2) The max of a list is greater than or equal to all elements of the list.
// (3) The max of a list is an element of that list.
// (4) The max of the empty list is unspecified and should throw an error or return `None`.

// Exercise 8.3
trait Prop {
  def check: Boolean

  def &&(p: Prop): Prop = new Prop {
    override def check: Boolean = Prop.this.check && p.check
  }
}

object Prop {
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

case class Gen[+A](sample: State[RNG, A]) {
  // Exercise 8.6
  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    Gen(sample.flatMap(f(_).sample))
  }

  def listOfN(size: Gen[Int]): Gen[List[A]] = {
    size.flatMap { i =>
      Gen(State.sequenceTextbook(List.fill(i)(this.sample)))
    }
  }

  // Textbook answer
  // A method alias for the function we wrote earlier.
  def listOfN(size: Int): Gen[List[A]] =
    Gen.listOfN(size, this)

  def listOfNTextbook(size: Gen[Int]): Gen[List[A]] =
    size flatMap (n => this.listOfN(n))
}

object Gen {
  // Exercise 8.4
  def intInRange(start: Int, stopExclusive: Int)(rng: RNG): (Int, RNG) = {
    val (nextInt, nextRng) = RNG.nonNegativeInt(rng)
    val range = stopExclusive - start
    ((nextInt % range) + start, nextRng)
  }

  def intInRange2(start: Int, stopExclusive: Int)(rng: RNG): (Int, RNG) = {
    RNG.nonNegativeInt(rng) match {
      case (n, rng2) => (start + n % (stopExclusive - start), rng2)
    }
  }

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(State(intInRange(start, stopExclusive)))
  }

  def choose2(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(State(intInRange2(start, stopExclusive)))
  }

  // Textbook answers
  def chooseTextbook(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

  // We could write this as an explicit state action, but this is far less
  // convenient, since it requires us to manually thread the `RNG` through
  // the computation.
  def chooseTextbook2(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(rng => RNG.nonNegativeInt(rng) match {
      case (n, rng2) => (start + n % (stopExclusive - start), rng2)
    }))

  // Exercise 8.5
  def unit[A](a: => A): Gen[A] = Gen(State(s => (a, s)))

  def unitTextbook[A](a: => A): Gen[A] =
    Gen(State.unit(a))

  def boolean: Gen[Boolean] = Gen(State(RNG.nonNegativeInt).map(n => if (n % 2 == 0) true else false))

  def booleanTextbook: Gen[Boolean] = Gen(State(RNG.boolean))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    def listOfN(count: Int, g: Gen[A])(rng: RNG): (List[A], RNG) =
      if (count <= 0)
        (List(), rng)
      else {
        val (a, r1) = g.sample.run(rng)
        val (as, r2) = listOfN(count - 1, g)(r1)
        (a :: as, r2)
      }

    Gen(State(rng =>
      listOfN(n, g)(rng)
    ))
  }

  def listOfNTextbook[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequenceTextbook(List.fill(n)(g.sample)))

  // Experimentation

  // (Q1) If we can generate a single int in some range, do we need a
  // new primitive to generate an (Int, Int) pair in some range?

  // New primitive required
  def map[A, B](g: Gen[A])(f: A => B): Gen[B] =
    Gen(g.sample.map(f))

  def choosePair(start: Int, stopExclusive: Int): Gen[(Int, Int)] =
    map(listOfNTextbook(2, choose(start, stopExclusive)))(l => (l(0), l(1)))

  // (Q2) Can we produce a Gen[Option[A]] from a Gen[A]
  def toOption[A](g: Gen[A]): Gen[Option[A]] =
    map(g)(Some(_))

  // (Q3) Can we produce a Gen[A] from a Gen[Option[A]]
  def fromOption[A](g: Gen[Option[A]], default: A): Gen[A] =
    map(g)(_.getOrElse(default))

  // (Q4) Can we generate strings somehow using our existing primitives?
  private val chars = (' ' to '~').toList

  val charGen: Gen[Char] = map(chooseTextbook(0, chars.size))(chars(_))

  def stringGen(length: Int): Gen[String] = map(listOfNTextbook(length, charGen))(_.mkString)

  // Exercise 8.6 - note, now moved into GenV1 class
  def flatMap[A, B](g: Gen[A])(f: A => Gen[B]): Gen[B] = {
    Gen(g.sample.flatMap(f(_).sample))
  }

  // Exercise 8.7
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
    Gen(State { rng =>
      val (coin, r1) = booleanTextbook.sample.run(rng)
      if (coin) g1.sample.run(r1) else g2.sample.run(r1)
    })
  }

  def unionTextbook[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if (b) g1 else g2)

  // Exercise 8.8
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    def g1Pct = g1._2 / (g1._2 + g2._2)
    def g1Cutoff = (g1Pct * 10).toInt

    chooseTextbook(1, 11).flatMap(i => if (i <= g1Cutoff) g1._1 else g2._1)
  }

  def weightedTextbook[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = {
    val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)

    Gen(State(RNG.double).flatMap(d =>
      if (d < g1Threshold) g1._1.sample else g2._1.sample
    ))
  }
}

object Workout {
  // Main loop
  def main(args: Array[String]): Unit = {
    println("Random characters:")
    for (a <- 1 to 10) {
      println(charGen.sample.run(Simple(a))._1)
    }

    println("Random strings of same length:")
    for (a <- 1 to 10) {
      println(stringGen(5).sample.run(Simple(a))._1)
    }

    println("Random strings of growing length:")
    for (a <- 1 to 10) {
      println(stringGen(a).sample.run(Simple(a))._1)
    }

    val gen1 = unit(1)
    val genOne = unit("one")

    val gen1OrOne = union(gen1, genOne)
    val gen1OrOneTextbook = unionTextbook(gen1, genOne)

    println("1 or one, equal weight:")
    for (a <- 1 to 10) {
      val value1 = gen1OrOne.sample.run(Simple(a))._1
      val value2 = gen1OrOneTextbook.sample.run(Simple(a))._1

      println(s"$value1, $value2")
    }

    println("1 or one, different weights:")
    val weight1 = 6
    val weight2 = 4
    val gen1OrOneWeighted = weighted((gen1, weight1), (genOne, weight2))
    val gen1OrOneWeightedTextbook = weightedTextbook((gen1,weight1), (genOne, weight2))
    for (a <- 1 to 10) {
      val value1 = gen1OrOneWeighted.sample.run(Simple(a))._1
      val value2 = gen1OrOneWeightedTextbook.sample.run(Simple(a))._1

      println(s"$value1, $value2")
    }
  }
}