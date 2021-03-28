package fpinscala.state

import com.typesafe.scalalogging.LazyLogging
import fpinscala.state.State.unit

import scala.annotation.tailrec

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG extends LazyLogging {

  // NB - this was called SimpleRNG in the book text
  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      logger.debug(s"nextInt called, seed [$seed] => newSeed [$newSeed] => n [$n]")
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  // Exercise 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (nextInt, nextRng) = rng.nextInt
    nextInt match {
      case x if x > 0 => (nextInt, nextRng)
      case x if x == Int.MinValue => (Int.MaxValue, nextRng)
      case _ => (-nextInt, nextRng)
    }
  }

  // Required for exercise 8.5
  def boolean(rng:RNG): (Boolean, RNG) = {
    val (i, r1) = nonNegativeInt(rng)
    val b = if (i % 2 == 0) true else false
    (b, r1)
  }

  // We need to be quite careful not to skew the generator.
  // Since `Int.Minvalue` is 1 smaller than `-(Int.MaxValue)`,
  // it suffices to increment the negative numbers by 1 and make them positive.
  // This maps Int.MinValue to Int.MaxValue and -1 to 0.
  def nonNegativeIntTextbook(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  // Exercise 6.2
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeIntTextbook(rng)
    ((if (i == Int.MaxValue) i - 1 else i).toDouble / Int.MaxValue, r)
  }

  // We generate an integer >= 0 and divide it by one higher than the
  // maximum. This is just one possible solution.
  def doubleTextbook(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  // Exercise 6.3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, rng2) = double(rng)
    val (i, rng3) = nonNegativeInt(rng2)
    ((d, i), rng3)
  }

  def doubleIntTextbook(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  // Exercise 6.4
  // Returns items in reverse order
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def go(count: Int)(rng: RNG)(acc: List[Int]): (List[Int], RNG) = {
      count match {
        case x if x <= 0 => (acc, rng)
        case _ => {
          val (i, r) = rng.nextInt
          go(count - 1)(r)(i +: acc)
        }
      }
    }

    go(count)(rng)(List.empty)
  }

  // Returns items in reverse order
  def intsTextbookTailRecursive(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def go(count: Int, r: RNG, xs: List[Int]): (List[Int], RNG) =
      if (count <= 0)
        (xs, r)
      else {
        val (x, r2) = r.nextInt
        go(count - 1, r2, x :: xs)
      }

    go(count, rng, List())
  }

  // Returns items in correct order
  def intsTextbookNotTailRecursive(count: Int)(rng: RNG): (List[Int], RNG) =
    if (count <= 0)
      (List(), rng)
    else {
      val (x, r1) = rng.nextInt
      val (xs, r2) = intsTextbookNotTailRecursive(count - 1)(r1)
      (x :: xs, r2)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  // Exercise 6.5
  def doubleViaMap(rng: RNG): (Double, RNG) = {
    map(_.nextInt)(
      i => {
        val i1 = if (i < 0) -(i + 1) else i
        (if (i1 == Int.MaxValue) i1 - 1 else i1).toDouble / Int.MaxValue
      }
    )(rng)
  }

  val doubleViaMapTextbook: Rand[Double] =
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  // Exercise 6.6

  // Start by accepting an RNG. Note that you have a choice in which RNG to pass to which function, and in what order.
  // Think about what you expect the behavior to be, and whether your implementation meets that expectation.

  // This implementation of map2 passes the initial RNG to the first argument
  // and the resulting RNG to the second argument. It's not necessarily wrong
  // to do this the other way around, since the results are random anyway.
  // We could even pass the initial RNG to both `f` and `g`, but that might
  // have unexpected results. E.g. if both arguments are `RNG.int` then we would
  // always get two of the same `Int` in the result. When implementing functions
  // like this, it's important to consider how we would test them for
  // correctness.
  private def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    r =>
      val (a, r1) = ra(r)
      logger.debug(s"a=[$a]")
      val (b, r2) = rb(r1)
      logger.debug(s"a=[$a], b=[$b]")
      (f(a, b), r2)
  }

  private def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  def intDoubleViaBoth: Rand[(Int, Double)] = {
    both(int, double)
  }

  def doubleIntViaBoth: Rand[(Double, Int)] = {
    both(double, int)
  }

  // Exercise 6.7

  def intsViaSequence(sequence: List[Rand[Int]] => Rand[List[Int]])(count: Int)(rng: RNG): (List[Int], RNG) = {
    sequence(List.fill(count)(int))(rng)
  }

  // You need to recursively iterate over the list. Remember that you can use
  // `foldLeft` or `foldRight` instead of writing a recursive definition. You
  // can also reuse the `map2` function you just wrote.

  // Note that the `RNG` value is not mentioned in `sequence`.
  // This suggests that we could make this function polymorphic in that type.

  // TODO - Not sure why this doesn't return items in correct order
  def sequenceFoldRight[A](fs: List[Rand[A]]): Rand[List[A]] = {
    r =>
      fs.foldRight((List.empty[A], r)) {
        case (randA, (acc, r1)) =>
          logger.debug(s"[$acc]")
          val (a, r2) = randA(r1)
          logger.debug(s"[$a] :: [$acc]")
          (a :: acc, r2)
      }
  }

  def sequenceFoldLeft[A](fs: List[Rand[A]]): Rand[List[A]] = {
    r =>
      val res =
        fs.foldLeft((List.empty[A], r)) {
          case ((acc, r1), randA) =>
            val (a, r2) = randA(r1)
            logger.debug(s"[$a] :: [$acc]")
            (a :: acc, r2)
        }
      (res._1.reverse, res._2)
  }

  // In `sequence`, the base case of the fold is a `unit` action that returns
  // the empty list. At each step in the fold, we accumulate in `acc` and `f`
  // is the current element in the list. `map2(f, acc)(_ :: _)` results in a
  // value of type `Rand[List[A]]`. We map over that to prepend (cons) the
  // element onto the accumulated list. We are using `foldRight`.

  // Returns items in correct order
  def sequenceTextbookFoldRight[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  // If we used `foldLeft` then the values in the resulting list appear in
  // reverse order. It would be arguably better to use `foldLeft` followed
  // by `reverse`.

  // Returns items in correct order
  def sequenceTextbookFoldLeft[A](fs: List[Rand[A]]): Rand[List[A]] =
    map(fs.foldLeft(unit(List[A]()))((acc, f) => map2(acc, f)((xs, x) => x :: xs)))(_.reverse)

  def nonNegativeLessThanSkewed(n: Int): Rand[Int] = map(nonNegativeInt)(_ % n)

  // Doesn't work, wrong type...
  //  def nonNegativeLessThan(n: Int): Rand[Int] =
  //    map(nonNegativeInt) { i =>
  //      val mod = i % n
  //      if (i + (n - 1) - mod >= 0) mod else nonNegativeLessThanTake2(n)
  //    }

  def nonNegativeLessThan(n: Int): Rand[Int] = { rng =>
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if (i + (n - 1) - mod >= 0)
      (mod, rng2)
    else nonNegativeLessThan(n)(rng2)
  }

  // Exercise 6.8
  // The implementation using `flatMap` will be almost identical to the failed one where we tried to use `map`.
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2) // We pass the new state along
    }

  def nonNegativeLessThanFlatMap(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0)
        unit(mod)
      else
        nonNegativeLessThan(n)
    }
  }

  // Exercise 6.9 - part 1
  def mapViaFlatMap[A, B](f: Rand[A])(g: A => B): Rand[B] =
    r => {
      val (a, r1) = f(r)
      flatMap(unit(a))(a => unit(g(a)))(r1)
    }

  def sequenceTextbookFoldLeftViaMapViaFlatMap[A](fs: List[Rand[A]]): Rand[List[A]] =
    mapViaFlatMap(fs.foldLeft(unit(List[A]()))((acc, f) => map2(acc, f)((xs, x) => x :: xs)))(_.reverse)

  def mapViaFlatMap2[A, B](f: Rand[A])(g: A => B): Rand[B] =
    flatMap(f)(a => unit(g(a)))

  def sequenceTextbookFoldLeftViaMapViaFlatMap2[A](fs: List[Rand[A]]): Rand[List[A]] =
    mapViaFlatMap2(fs.foldLeft(unit(List[A]()))((acc, f) => map2(acc, f)((xs, x) => x :: xs)))(_.reverse)

  // Exercise 6.9 - part 2
  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra)(a => flatMap(rb)(b => unit(f(a, b))))
  }

  def sequenceTextbookFoldRightViaMap2ViaFlatMap[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2ViaFlatMap(f, acc)(_ :: _))

  def map2ViaFlatMapTextbook[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra)(a => map(rb)(b => f(a, b)))
  }

  def sequenceTextbookFoldRightViaMap2ViaFlatMapTextbook[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2ViaFlatMapTextbook(f, acc)(_ :: _))

  def incorrectRollDie: Rand[Int] = nonNegativeLessThan(6)

  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)
}

case class State[S, +A](run: S => (A, S)) {

  // def map[S, A, B](s: S => (A, S))(f: A => B): S => (B, S)

  // type State[S, +A] = S => (A, S)

  // type Rand[+A] = State[RNG, A]

  def map[B](f: A => B): State[S, B] = State(
    s => {
      val (a, s1) = run(s)
      (f(a), s1)
    }
  )

  def mapTextbook[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = State(
    s => {
      val (a, s1) = run(s)
      val (b, s2) = sb.run(s1)
      (f(a, b), s2)
    }
  )

  def map2Textbook[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(
    s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    }
  )
}

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def sequenceViaFoldRight[S, A](l: List[State[S, A]]): State[S, List[A]] = {
    l.foldRight(unit[S, List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))
  }

  // This implementation uses a loop internally and is the same recursion
  // pattern as a left fold. It is quite common with left folds to build
  // up a list in reverse order, then reverse it at the end.
  // (We could also use a collection.mutable.ListBuffer internally.)
  def sequenceTextbook[S, A](stateList: List[State[S, A]]): State[S, List[A]] = {
    def go(s: S, actions: List[State[S, A]], acc: List[A]): (List[A], S) =
      actions match {
        case Nil => (acc.reverse, s)
        case h :: t => h.run(s) match {
          case (a, s2) => go(s2, t, a :: acc)
        }
      }

    State((s: S) => go(s, stateList, List()))
  }

  def sequenceViaFoldLeft[S, A](l: List[State[S, A]]): State[S, List[A]] = {
    l.foldLeft(unit[S, List[A]](List()))((acc, f) => f.map2(acc)(_ :: _)).map(_.reverse)
  }

  // We can also write the loop using a left fold. This is tail recursive like the
  // previous solution, but it reverses the list _before_ folding it instead of after.
  // You might think that this is slower than the `foldRight` solution since it
  // walks over the list twice, but it's actually faster! The `foldRight` solution
  // technically has to also walk the list twice, since it has to unravel the call
  // stack, not being tail recursive. And the call stack will be as tall as the list
  // is long.
  def sequenceViaFoldLeftTextbook[S, A](l: List[State[S, A]]): State[S, List[A]] =
    l.reverse.foldLeft(unit[S, List[A]](List()))((acc, f) => f.map2(acc)(_ :: _))

  private val int: State.Rand[Int] = State(_.nextInt)

  private def ints(count: Int): State.Rand[List[Int]] = {
    State.sequenceViaFoldRight(List.fill(count)(int))
  }

  private def nonNegativeLessThan(n: Int): State.Rand[Int] = State(rng => {
    val (i, rng2) = RNG.nonNegativeInt(rng)
    val mod = i % n
    if (i + (n - 1) - mod >= 0)
      (mod, rng2)
    else nonNegativeLessThan(n).run(rng2)
  })

  def numbers1: State.Rand[List[Int]] =
    nonNegativeLessThan(10).flatMap(x =>
      int.flatMap(y =>
        ints(x).map(xs =>
          xs.map(_ % y)
        )
      )
    )

  def numbers2: State.Rand[List[Int]] = for {
    x <- nonNegativeLessThan(10)
    y <- int
    xs <- ints(x)
  } yield xs.map(_ % y)

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

  def modify2[S](f: S => S): State[S, Unit] =
    get.flatMap(s => set(f(s)))
}

object CandyMachine {

  sealed trait Input

  case object Coin extends Input

  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int) {
    override def toString() = {
      s"Machine (locked: $locked, candies: $candies, coins: $coins)"
    }
  }

  def update(i: Input)(m: Machine): Machine = {
    (i, m) match {
      case (_, Machine(_, 0, _)) => m
      case (Coin, Machine(false, _, _)) => m
      case (Turn, Machine(true, _, _)) => m
      case (Coin, Machine(true, candy, coin)) => Machine(false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) => Machine(true, candy - 1, coin)
    }
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    val modifyMachine: (Machine => Machine) => State[Machine, Unit] = State.modify[Machine]
    val inputToState: Input => State[Machine, Unit] = modifyMachine compose update // update followed by modifyMachine
    val stateList: List[State[Machine, Unit]] = inputs map inputToState
    val finalState: State[Machine,List[Unit]] = State.sequenceTextbook(stateList)

    for {
      _ <- finalState
      s <- State.get
    } yield (s.coins, s.candies)
  }

  def simulateMachine2(inputs: List[Input]): State[Machine, (Int, Int)] = {
    val finalState: State[Machine,List[Unit]] =
      State.sequenceTextbook(inputs map (State.modify[Machine] _ compose update))

    finalState.flatMap(_ => State.get.map(s => (s.coins, s.candies)))
  }
}

object Workout {

  def main(args: Array[String]): Unit = {
    import RNG._

    println(nonNegativeEven(Simple(1)))

    // Exposes state in result output
    val a = State.get[Int]
    println(a.run(3)) // (3,3)

    // Sets result state, disregarding input and exposes no output in result
    val b = State.set(33)
    println(b.run(666)) // ((),33)

    val c = State[Int, Int](x => (x * 3, x * 2))
    println(c.run(5)) // (15,10)

    // Produces a state that takes takes the input state and modifies it by the supplied function
    val d = State.modify[Int](_ * 2)
    println(d.run(2)) // ((),4)
  }
}