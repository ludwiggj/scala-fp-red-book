package fpinscala.errorhandling

// hide std library `Option` and `Either`, since we are writing our own in this chapter

import scala.{Either => _, Left => _, Option => _, Right => _}

/*
There are a number of variations on `Option` and `Either`. If we want to accumulate multiple errors, a simple approach
is a new data type that lets us keep a list of errors in the data constructor that represents failures:

trait Partial[+A,+B]
case class Errors[+A](get: Seq[A]) extends Partial[A,Nothing]
case class Success[+B](get: B) extends Partial[Nothing,B]

There is a type very similar to this called `Validation` in the Scalaz library. You can implement `map`, `map2`,
`sequence`, and so on for this type in such a way that errors are accumulated when possible (`flatMap` is unable to
accumulate errors--can you see why?). This idea can even be generalized further--we don't need to accumulate failing
values into a list; we can accumulate values using any user-supplied binary function.

It's also possible to use `Either[List[E],_]` directly to accumulate errors, using different implementations of helper
functions like `map2` and `sequence`.
*/

sealed trait Eithery[+E, +A] {
  def map[B](f: A => B): Eithery[E, B] = this match {
    case Righty(a) => Righty(f(a))
    case Lefty(e) => Lefty(e)
  }

  def flatMap[EE >: E, B](f: A => Eithery[EE, B]): Eithery[EE, B] = this match {
    case Righty(a) => f(a)
    case Lefty(e) => Lefty(e)
  }

  def orElse[EE >: E, B >: A](b: => Eithery[EE, B]): Eithery[EE, B] = this match {
    case Righty(a) => Righty(a)
    case Lefty(e) => b match {
      case Righty(bb) => Righty(bb)
      case Lefty(ee) => Lefty(e ++ ee)
    }
  }

  def map2[EE >: E, B, C](b: Eithery[EE, B])(f: (A, B) => C): Eithery[EE, C] = this match {
    case Righty(a) => b match {
      case Righty(bb) => Righty(f(a, bb))
      case Lefty(e) => Lefty(e)
    }
    case Lefty(e) => b match {
      case Righty(_) => Lefty(e)
      case Lefty(ee) => Lefty(e ++ ee)
    }
  }
}

case class Lefty[+E](values: Seq[E]) extends Eithery[E, Nothing]

// Aux constructor for Lefty
object Lefty {
  def apply[E](value: E): Eithery[E, Nothing] = Lefty(Seq(value))
}

case class Righty[+A](value: A) extends Eithery[Nothing, A]

object Eithery {
  // Exercise 4.7
  def traverse[E, A, B](es: List[A])(f: A => Eithery[E, B]): Eithery[E, List[B]] =
    es.foldRight[Eithery[E, List[B]]](Righty(Nil))((a, b) => f(a).map2(b)(_ :: _))

  def sequence[E, A](es: List[Eithery[E, A]]): Eithery[E, List[A]] =
    traverse(es)(identity)

  // Example functions
  def mean(xs: IndexedSeq[Double]): Eithery[String, Double] =
    if (xs.isEmpty)
      Lefty("mean of empty list!")
    else
      Righty(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Eithery[Exception, Int] =
    try Righty(x / y)
    catch {
      case e: Exception => Lefty(e)
    }

  def Try[A](a: => A): Eithery[Exception, A] =
    try Righty(a)
    catch {
      case e: Exception => Lefty(e)
    }

  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = (100 - age) * (1 + numberOfSpeedingTickets)

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Eithery[Exception, Double] = {
    Try(age.toInt).map2(Try(numberOfSpeedingTickets.toInt))(insuranceRateQuote)
  }

  case class Person(name: Name, age: Age)

  sealed class Name(val value: String) {
    override def toString: String = s"Name($value)"
  }

  sealed class Age(val value: Int) {
    override def toString: String = s"Age($value)"
  }

  def mkName(name: String): Eithery[String, Name] =
    if (name == "" || name == null) Lefty("Name is empty.")
    else Righty(new Name(name))

  def mkAge(age: Int): Eithery[String, Age] =
    if (age < 0) Lefty("Age is out of range.")
    else Righty(new Age(age))

  def mkPerson(name: String, age: Int): Eithery[String, Person] =
    mkName(name).map2(mkAge(age))(Person)

}

object EitheryTests {

  import Eithery._

  def initialTests(): Unit = {
    println("Initial Tests")
    println("=============")

    println(s"mean(Seq()) = ${mean(IndexedSeq())}")
    println(s"mean(Seq(1, 3, 5)) = ${mean(IndexedSeq(1, 3, 5))}")

    println(s"safeDiv(4, 2) = ${safeDiv(4, 2)}")
    println(s"safeDiv(4, 0) = ${safeDiv(4, 0)}")

    println(s"Try(4/2) = ${Try(4 / 2)}")
    println(s"Try(4/0) = ${Try(4 / 0)}")

    println()
  }

  def testExercise_4_6(): Unit = {
    println("Exercise 4.6, either")
    println("====================")

    println("map")
    println("---")
    println(s"Try(4 / 2).map(_ * 10) = ${Try(4 / 2).map(_ * 10)}")
    println(s"Try(4 / 0).map(_ * 10) = ${Try(4 / 0).map(_ * 10)}")
    println()

    println("flatMap")
    println("-------")
    println(s"Try(4 / 2).flatMap(x => Try(x * 10)) = ${Try(4 / 2).flatMap(x => Try(x * 10))}")
    println(s"Try(4 / 2).flatMap(x => Try(x / 0)) = ${Try(4 / 2).flatMap(x => Try(x / 0))}")
    println(s"Try(4 / 0).flatMap(x => Try(x * 10)) = ${Try(4 / 0).flatMap(x => Try(x * 10))}")
    println(s"Try(4 / 0).flatMap(x => Try(x / 0)) = ${Try(4 / 0).flatMap(x => Try(x / 0))}")
    println()

    println("orElse")
    println("------")
    println(s"""Try(4 / 2).map(_ * 10).orElse(Righty("orElse!")) = ${Try(4 / 2).map(_ * 10).orElse(Righty("orElse!"))}""")
    println(s"""Try(4 / 0).map(_ * 10).orElse(Righty("orElse!")) = ${Try(4 / 0).map(_ * 10).orElse(Righty("orElse!"))}""")
    println(s"""Try(4 / 2).map(_ * 10).orElse(Try("six".toInt)) = ${Try(4 / 2).map(_ * 10).orElse(Try("six".toInt))}""")
    println(s"""Try(4 / 0).map(_ * 10).orElse(Try("six".toInt)) = ${Try(4 / 0).map(_ * 10).orElse(Try("six".toInt))}""")
    println()

    println("map2")
    println("----")
    println(s"Try(4 / 2).map2(Try(10 / 2))(_ * _) = ${Try(4 / 2).map2(Try(10 / 2))(_ * _)}")
    println(s"Try(4 / 0).map2(Try(10 / 2))(_ * _) = ${Try(4 / 0).map2(Try(10 / 2))(_ * _)}")
    println(s"Try(4 / 2).map2(Try(10 / 0))(_ * _) = ${Try(4 / 2).map2(Try(10 / 0))(_ * _)}")
    println(s"""Try(4 / 0).map2(Try("seven".toInt))(_ * _) = ${Try(4 / 0).map2(Try("seven".toInt))(_ * _)}""")
    println()
  }

  def testParseInsuranceRateQuote(): Unit = {
    println("parseInsuranceRateQuote")
    println("=======================")

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


  def testExercise_4_7(): Unit = {
    println("sequence")
    println("========")

    println(s"sequence of List() is ${sequence(List())}")
    println(s"sequence of List(Try(4 / 2), Try(4 * 2)) is ${sequence(List(Try(4 / 2), Try(4 * 2)))}")
    println(s"""sequence of List(Try(4 / 0), Try(4 * 2), Try("three".toInt)) is ${sequence(List(Try(4 / 0), Try(4 * 2), Try("three".toInt)))}""")
    println(s"""sequence of List(Try("four".toInt), Try(4 * 2), Try(3 / 0)) is ${sequence(List(Try("four".toInt), Try(4 * 2), Try(3 / 0)))}""")
    println()

    println("traverse")
    println("========")

    println(s"traverse of List[String]()(s => Try(s.toInt)) is ${traverse(List[String]())(s => Try(s.toInt))}")
    println(s"""traverse of List[String]("1", "2", "3")(s => Try(s.toInt)) is ${traverse(List[String]("1", "2", "3"))(s => Try(s.toInt))}""")
    println(s"""traverse of List[String]("1", "two", "3")(s => Try(s.toInt)) is ${traverse(List[String]("1", "two", "3"))(s => Try(s.toInt))}""")
    println(s"""traverse of List[String]("one", "2", "three")(s => Try(s.toInt)) is ${traverse(List[String]("one", "2", "three"))(s => Try(s.toInt))}""")
    println()
  }

  def testPerson(): Unit = {
    println("person")
    println("======")

    println(s"""mkName("") = ${mkName("")}""")
    println(s"""mkName(null) = ${mkName(null)}""")
    println(s"""mkName("Graeme") = ${mkName("Graeme")}""")

    println(s"""mkAge(-1) = ${mkAge(-1)}""")
    println(s"""mkAge(0) = ${mkAge(0)}""")
    println(s"""mkAge(1) = ${mkAge(1)}""")

    println(s"""mkPerson("Graeme", 50) = ${mkPerson("Graeme", 50)}""")
    println(s"""mkPerson("", 50) = ${mkPerson("", 50)}""")
    println(s"""mkPerson("Graeme", -2) = ${mkPerson("Graeme", -2)}""")
    println(s"""mkPerson(null, -5) = ${mkPerson(null, -5)}""")
  }

  def main(args: Array[String]): Unit = {
    initialTests()

    testExercise_4_6()

    testParseInsuranceRateQuote()

    testExercise_4_7()

    testPerson()
  }
}