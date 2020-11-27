package fpinscala.errorhandling

// hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

import scala.{+:, Either => _, Option => _, Some => _, _}

sealed trait Option[+A] {
  // Exercise 4.1
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case _ => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case _ => default
  }

  def get(): A = this match {
    case Some(a) => a
    case _ => throw new Exception("Cannot call get on None")
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  def flatMap2[B](f: A => Option[B]): Option[B] = this match {
    case Some(a) => f(a)
    case _ => None
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    // Wrap a Some around the value just to remove it later!
    // I prefer the pattern matching version: the intent is clearer
    map(Some(_)) getOrElse ob
  }

  def orElse2[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }

  def filter(f: A => Boolean): Option[A] =
    flatMap(a => if (f(a)) Some(a) else None)

  def filter2(f: A => Boolean): Option[A] = this match {
    case Some(a) if (f(a)) => this
    case _ => None
  }
}

case class Some[+A](a: A) extends Option[A]

case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    val y: Int = throw new Exception("fail!")
    try {
      val x = 42 + 5
      x + y
    }
      // A `catch` block is just a pattern matching block like the ones we've seen.
      // `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`.
      // The match returns the value 43.
    catch {
      case e: Exception => 43
    }
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      // A thrown Exception can be given any type; here we're annotating it with the type `Int`
      x + ((throw new Exception("fail!")): Int)
    }
    catch {
      case e: Exception => 43
    }
  }

  object Exercise_4_2 {
    def mean(xs: Seq[Double]): Option[Double] =
      if (xs.isEmpty) None
      else Some(xs.sum / xs.length)


    def variance(xs: Seq[Double]): Option[Double] = {
      mean(xs).flatMap(m =>
        mean(xs.map(x => math.pow(x - m, 2)))
      )
    }
  }

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  object Exercise_4_3 {
    def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a map (aa => b map (bb => f(aa, bb))) getOrElse (None)

    def map2Textbook[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
      a flatMap (aa => b map (bb => f(aa, bb)))
  }

  // Lifting function in the context of 2 options
  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = (100 - age) * (1 + numberOfSpeedingTickets)

  def Try[A](a: => A): Option[A] = // Non-strict parameter
    try Some(a)
    catch {
      case e: Exception => None // Error information discarded
    }

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
    import Exercise_4_3.map2

    val optAge: Option[Int] = Try(age.toInt)
    val optTickets: Option[Int] = Try(numberOfSpeedingTickets.toInt)

    map2(optAge, optTickets)(insuranceRateQuote)
  }

  // Exercise 4.4
  object Exercise_4_4 {

    import Exercise_4_3.map2

    def sequence[A](a: List[Option[A]]): Option[List[A]] =
      if (a.filter(_ == None).size > 0) None else Some(a.map(_.get))

    // Break the list out using pattern-matching where there will be a recursive call to `sequence` in the cons case.
    def sequenceTextbook1[A](a: List[Option[A]]): Option[List[A]] = a match {
      case x :: xs => x flatMap (xx => sequenceTextbook1(xs).map(xx :: _))
      case _ => Some(Nil)
    }

    // Alternatively, use the `foldRight` method to take care of the recursion for you
    def sequenceTextbook2[A](a: List[Option[A]]): Option[List[A]] =
      a.foldRight(Some(List[A]()): Option[List[A]])((a, b) => {
        a flatMap (aa => b map (bb => aa :: bb))
      })

    // It can also be implemented using `foldRight` and `map2`. The type annotation on `foldRight` is needed here;
    // otherwise Scala wrongly infers the result type of the fold as `Some[Nil.type]` and reports a type error
    // (try it!). This is an unfortunate consequence of Scala using subtyping to encode algebraic data types
    def sequenceTextbook3[A](a: List[Option[A]]): Option[List[A]] =
      a.foldRight[Option[List[A]]](Some(Nil))((x, y) => map2(x, y)(_ :: _))
  }

  object Exercise_4_4_BespokeList {

    import fpinscala.datastructures.{List, Cons, Nil}

    // Break the list out using pattern-matching where there will be a recursive call to `sequence` in the cons case.
    def sequenceTextbook1[A](a: List[Option[A]]): Option[List[A]] = a match {
      case Cons(x, xs) => {
        x flatMap (xx => sequenceTextbook1(xs).map(Cons(xx, _)))
      }
      case _ => Some(Nil)
    }

    // Alternatively, use the `foldRight` method to take care of the recursion for you

    import fpinscala.datastructures.List.foldRight

    def sequenceTextbook2[A](a: List[Option[A]]): Option[List[A]] =
      foldRight(a, Some(List[A]()): Option[List[A]])((a, b) => {
        a flatMap (aa => b map (bb => Cons(aa, bb)))
      })
  }

  def parseInts(a: List[String]): Option[List[Int]] = {
    import Exercise_4_4.sequence

    sequence(a map (i => Try(i.toInt)))
  }

  object Exercise_4_5 {

    import Exercise_4_3.map2
    import Exercise_4_4.sequence

    def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
      sequence(a map f)
    }

    // The `traverse` function can be written with explicit recursion
    def traverseEfficient[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
      case x :: xs => map2(f(x), traverseEfficient(xs)(f))(_ :: _)
      case _ => Some(Nil)
    }

    // or use `foldRight` to do the recursion for you.
    def traverseFoldRight[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
      a.foldRight(Some(Nil): Option[List[B]])((a, b) => map2(f(a), b)(_ :: _))
    }

    // Implement `sequence` using `traverse`: it may be more trivial than you think!
    // def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]]
    // def sequence[A](a: List[Option[A]]): Option[List[A]]
    def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] =
      traverse(a)(identity)
  }

  // Test functions
  def testFailingFunction(fnName: String, fn: Int => Int): Unit = {
    println("failingFunction")
    println("===============")

    println(s"Result of $fnName(5) is [${fn(5)}]")
    println()
  }

  def testExercise_4_1(): Unit = {
    println("Exercise 4.1, option methods")
    println("============================")

    val something = Some(5)
    val nothing = None

    def testOption(option: Option[Int]): Unit = {
      println(s"$option.map(_ * 2) = ${option.map(_ * 2)}")
      println(s"$option.getOrElse(10) = ${option.getOrElse(10)}")
      println(s"$option.flatMap(a => Some('£' * a)) = ${option.flatMap(a => Some("£" * a))}")
      println(s"$option.flatMap2(a => Some('£' * a)) = ${option.flatMap2(a => Some("£" * a))}")
      println(s"$option.orElse(Some(255d)) = ${option.orElse(Some(255d))}")
      println(s"$option.orElse2(Some(255d)) = ${option.orElse2(Some(255d))}")
      println(s"$option.filter(_ < 6) = ${option.filter(_ < 6)}")
      println(s"$option.filter2(_ > 6) = ${option.filter2(_ > 6)}")
      println()
    }

    testOption(something)
    testOption(nothing)
  }

  def testExercise_4_2(): Unit = {
    import Exercise_4_2._

    println("Exercise 4.2, variance")
    println("======================")

    def testVariance(xs: Seq[Double]): Unit = {
      println(s"Variance of $xs is ${variance(xs)}")
    }

    testVariance(Seq(2, 4, 6, 8))
    testVariance(Seq())
    println()
  }

  def testLift(): Unit = {
    println("lift")
    println("====")

    val abs0: Option[Double] => Option[Double] = lift(math.abs)

    def testAbs(o: Option[Double]): Unit = {
      println(s"abs($o) = ${abs0(o)}")
    }

    testAbs(Some(15))
    testAbs(Some(-12))
    testAbs(Some(0))
    testAbs(None)
    println()
  }

  def testExercise_4_3(): Unit = {
    println("Exercise 4.3, map2")
    println("==================")

    def testParseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Unit = {
      println(s"insuranceQuote(age = [$age], numberOfSpeedingTickets = [$numberOfSpeedingTickets]) = " +
        parseInsuranceRateQuote(age, numberOfSpeedingTickets))
    }

    testParseInsuranceRateQuote("20", "2")
    testParseInsuranceRateQuote("50", "0")
    testParseInsuranceRateQuote("twenty-two", "0")
    testParseInsuranceRateQuote("22", "zero")
    testParseInsuranceRateQuote("oh", "dear")

    println()
  }

  def testSequence[A](sequenceFnName: String, sequence: List[Option[Any]] => Option[List[Any]], a: List[Option[A]]): Unit = {
    println(s"$sequenceFnName of $a is ${sequence(a)}")
  }

  def testSequences[A](sequenceFnName: String, sequence: List[Option[Any]] => Option[List[Any]]): Unit = {
    testSequence(sequenceFnName, sequence, List())
    testSequence(sequenceFnName, sequence, List(Some("1"), Some("2"), Some("3")))
    testSequence(sequenceFnName, sequence, List(Some(2), None, Some(3)))
  }

  def testExercise_4_4(): Unit = {
    import Exercise_4_4._

    println("Exercise 4.4, sequence")
    println("======================")

    testSequences("sequence", sequence)
    testSequences("sequenceTextbook1", sequenceTextbook1)
    testSequences("sequenceTextbook2", sequenceTextbook2)
    testSequences("sequenceTextbook3", sequenceTextbook3)
    println()
  }

  def testExercise_4_4_BeskpokeList(): Unit = {
    import Exercise_4_4_BespokeList._

    println("Exercise 4.4, sequence (BespokeList)")
    println("====================================")

    import fpinscala.datastructures.List

    def testSequence[A](sequenceFnName: String, sequence: List[Option[Any]] => Option[List[Any]], a: List[Option[A]]): Unit = {
      println(s"$sequenceFnName of $a is ${sequence(a)}")
    }

    def testSequences[A](sequenceFnName: String, sequence: List[Option[Any]] => Option[List[Any]]): Unit = {
      testSequence(sequenceFnName, sequence, List())
      testSequence(sequenceFnName, sequence, List(Some("1"), Some("2"), Some("3")))
      testSequence(sequenceFnName, sequence, List(Some(2), None, Some(3)))
    }

    testSequences("sequenceTextbook1", sequenceTextbook1)
    testSequences("sequenceTextbook2", sequenceTextbook2)
    println()
  }

  def testParseInts(): Unit = {
    println("parseInts")
    println("=========")

    def testParseInts(a: List[String]) = {
      println(s"parseInts($a) = ${parseInts(a)}")
    }

    testParseInts(List("2", "4", "6"))
    testParseInts(List("2", "4.5", "6"))
    testParseInts(List("2", "Yo", "6"))

    println()
  }

  def testExercise_4_5(): Unit = {
    import Exercise_4_5._

    println("Exercise 4.5, traverse")
    println("======================")

    def testTraverse(traverseFnName: String, traverse: List[String] => (String => Option[Int]) => Option[List[Int]], a: List[String]): Unit = {
      println(s"$traverseFnName($a) = ${traverse(a)(i => Try(i.toInt))}")
    }

    def testTraverses(traverseFnName: String, traverse: List[String] => (String => Option[Int]) => Option[List[Int]]): Unit = {
      testTraverse(traverseFnName, traverse, List("1", "2", "3"))
      testTraverse(traverseFnName, traverse, List("1.6", "2", "3"))
      testTraverse(traverseFnName, traverse, List("1", "2", "Oy!"))
    }

    testTraverses("traverse", traverse)
    testTraverses("traverseEfficient", traverseEfficient)
    testTraverses("traverseFoldRight", traverseFoldRight)

    testSequences("sequenceViaTraverse", sequenceViaTraverse)

    println()
  }

  def main(args: Array[String]): Unit = {
    // testFailingFunction("failingFn", failingFn)
    testFailingFunction("failingFn2", failingFn2)
    testExercise_4_1()
    testExercise_4_2()
    testLift()
    testExercise_4_3()
    testExercise_4_4()
    testExercise_4_4_BeskpokeList()
    testParseInts()
    testExercise_4_5()
  }
}