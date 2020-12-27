package fpinscala.errorhandling

// hide std library `Option` and `Either`, since we are writing our own in this chapter

import scala.{Either => _, Left => _, Option => _, Right => _}

sealed trait Either[+E, +A] {
  // Exercise 4.6
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(a) => Right(f(a))
    case Left(e) => Left(e)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => f(a)
    case Left(e) => Left(e)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => Right(a)
    case Left(_) => b
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    this flatMap (aa => b map (bb => f(aa, bb)))

  def map2Textbook[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      aa <- this
      bb <- b
    } yield f(aa, bb)
}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  // Exercise 4.7
  def traverse[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es.foldRight(Right(List()): Either[E, List[B]])((a, b) => {
      for {
        aa <- f(a)
        bb <- b
      } yield aa :: bb
    })

  def traverse_2[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es.foldRight[Either[E, List[B]]](Right(Nil))((a, b) => f(a).map2(b)(_ :: _))

  def traverse_3[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es match {
      case Nil => Right(Nil)
      case x :: xs => f(x).map2(traverse_3(xs)(f))(_ :: _)
    }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    es.foldRight(Right(List()): Either[E, List[A]])((a, b) => {
      for {
        aa <- a
        bb <- b
      } yield aa :: bb
    })

  def sequence_2[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    es.foldRight[Either[E, List[A]]](Right(Nil))((a, b) => a.map2(b)(_ :: _))

  def sequence_3[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(identity)

  // Example functions
  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch {
      case e: Exception => Left(e)
    }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }

  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = (100 - age) * (1 + numberOfSpeedingTickets)

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Either[Exception, Double] = {
    for {
      a <- Try(age.toInt)
      tickets <- Try(numberOfSpeedingTickets.toInt)
    } yield insuranceRateQuote(a, tickets)
  }

  case class Person(name: Name, age: Age)

  sealed class Name(val value: String) {
    override def toString: String = s"Name($value)"
  }

  sealed class Age(val value: Int) {
    override def toString: String = s"Age($value)"
  }

  def mkName(name: String): Either[String, Name] =
    if (name == "" || name == null) Left("Name is empty.")
    else Right(new Name(name))

  def mkAge(age: Int): Either[String, Age] =
    if (age < 0) Left("Age is out of range.")
    else Right(new Age(age))

  def mkPerson(name: String, age: Int): Either[String, Person] =
    mkName(name).map2(mkAge(age))(Person)

  object Tests {
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
      println(s"""Try(4 / 2).map(_ * 10).orElse(Left("orElse!")) = ${Try(4 / 2).map(_ * 10).orElse(Right("orElse!"))}""")
      println(s"""Try(4 / 0).map(_ * 10).orElse(Left("orElse!")) = ${Try(4 / 0).map(_ * 10).orElse(Right("orElse!"))}""")
      println()

      println("map2")
      println("----")
      println(s"Try(4 / 2).map2(Try(10 / 2))(_ * _) = ${Try(4 / 2).map2(Try(10 / 2))(_ * _)}")
      println(s"Try(4 / 0).map2(Try(10 / 2))(_ * _) = ${Try(4 / 0).map2(Try(10 / 2))(_ * _)}")
      println(s"Try(4 / 2).map2(Try(10 / 0))(_ * _) = ${Try(4 / 2).map2(Try(10 / 0))(_ * _)}")
      println()

      println(s"Try(4 / 2).map2Textbook(Try(10 / 2))(_ * _) = ${Try(4 / 2).map2Textbook(Try(10 / 2))(_ * _)}")
      println(s"Try(4 / 0).map2Textbook(Try(10 / 2))(_ * _) = ${Try(4 / 0).map2Textbook(Try(10 / 2))(_ * _)}")
      println(s"Try(4 / 2).map2Textbook(Try(10 / 0))(_ * _) = ${Try(4 / 2).map2Textbook(Try(10 / 0))(_ * _)}")
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

      def testSequence(sequenceFnName: String, sequence: List[Either[Any, Any]] => Either[Any, List[Any]]): Unit = {
        println(s"$sequenceFnName of List() is ${sequence(List())}")
        println(s"$sequenceFnName of List(Try(4 / 2), Try(4 * 2)) is ${sequence(List(Try(4 / 2), Try(4 * 2)))}")
        println(s"""$sequenceFnName of List(Try(4 / 0), Try(4 * 2), Try("three".toInt)) is ${sequence(List(Try(4 / 0), Try(4 * 2), Try("three".toInt)))}""")
        println(s"""$sequenceFnName of List(Try("four".toInt), Try(4 * 2), Try(3 / 0)) is ${sequence(List(Try("four".toInt), Try(4 * 2), Try(3 / 0)))}""")
      }

      testSequence("sequence", sequence)
      testSequence("sequence_2", sequence_2)
      testSequence("sequence_3", sequence_3)
      println()

      println("traverse")
      println("========")

      def testTraverse(traverseFnName: String, traverse: List[String] => (String => Either[Exception, Int]) => Either[Exception, List[Int]]): Unit = {
        println(s"$traverseFnName of List[String]()(s => Try(s.toInt)) is ${traverse(List[String]())(s => Try(s.toInt))}")
        println(s"""$traverseFnName of List[String]("1", "2", "3")(s => Try(s.toInt)) is ${traverse(List[String]("1", "2", "3"))(s => Try(s.toInt))}""")
        println(s"""$traverseFnName of List[String]("1", "two", "3")(s => Try(s.toInt)) is ${traverse(List[String]("1", "two", "3"))(s => Try(s.toInt))}""")
        println(s"""$traverseFnName of List[String]("one", "2", "three")(s => Try(s.toInt)) is ${traverse(List[String]("one", "2", "three"))(s => Try(s.toInt))}""")
      }

      testTraverse("traverse", traverse)
      testTraverse("traverse_2", traverse_2)
      testTraverse("traverse_3", traverse_3)
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
  }

  def main(args: Array[String]): Unit = {
    import Either.Tests._

    initialTests()

    testExercise_4_6()

    testParseInsuranceRateQuote()

    testExercise_4_7()

    testPerson()
  }
}