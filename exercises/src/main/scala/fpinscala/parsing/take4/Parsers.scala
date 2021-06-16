package fpinscala.parsing.take4

import fpinscala.testing.take2.Prop.forAll
import fpinscala.testing.take2.{Gen, Prop}

import language.higherKinds
import scala.util.matching.Regex

// NOTE: Error reporting textbook answer (didn't attempt to specify it, exercises 9.10 & 9.11)

trait Parsers[Parser[+_]] {
  self => // so inner classes may call methods of trait

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)

  implicit def string(s: String): Parser[String]

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  implicit def char(c: Char): Parser[Char] =
    string(c.toString) map (_.charAt(0))

  implicit def asCharParser[A](a: A)(implicit f: A => Parser[Char]): ParserOps[Char] = ParserOps(f(a))

  implicit def regex(r: Regex): Parser[String]

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  def many[A](p: Parser[A]): Parser[List[A]]

  def map[A, B](a: Parser[A])(f: A => B): Parser[B]

  def slice[A](p: Parser[A]): Parser[String]

  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)]

  def many1[A](p: Parser[A]): Parser[List[A]]

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

  def succeed[A](a: A): Parser[A]

  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C]

  // Assign error message to a parser
  // If p fails, its ParseError will somehow incorporate msg
  // It will throw away any existing labels attached to the parser (see section 9.5.2, pg 162)
  def label[A](msg: String)(p: Parser[A]): Parser[A]

  // This is a way to nest labels. This method doesn't throw away labels attached to p
  // It merely adds additional information in the event that p fails
  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  // Attempt delays committing to a parse
  def attempt[A](p: Parser[A]): Parser[A]

  // Exercise 9.11, other primitives to specify what errors get reported in an or chain

  // Hint: Here are two options: we could return the most recent error in the `or` chain,
  // or we could return whichever error occurred after getting furthest into the input string.

  // i.e.

  // In the event of an error, returns the error that occurred after consuming the most number of characters.
  def furthest[A](p: Parser[A]): Parser[A]

  // In the event of an error, returns the error that occurred most recently.
  def latest[A](p: Parser[A]): Parser[A]

  case class Location(input: String, offset: Int = 0) {
    private lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
    private lazy val col = input.slice(0, offset + 1).lastIndexOf('\n') match {
      case -1 => offset + 1
      case lineStart => offset - lineStart
    }
  }

  case class ParseError(stack: List[(Location, String)])

  // Still unchanged after moving to concrete representation of ParseError
  def errorLocation(e: ParseError): Location

  // Still unchanged after moving to concrete representation of ParseError
  def errorMessage(e: ParseError): String

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def many: Parser[List[A]] = self.many(p)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def slice: Parser[String] = self.slice(p)

    def product[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def many1: Parser[List[A]] = self.many1(p)

    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)

    def listOfN(n: Int): Parser[List[A]] = self.listOfN(n, p)
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop = {
      equal(p, p.map(a => a))(in)
    }

    def unbiasL[A, B, C](p: ((A, B), C)): (A, B, C) = (p._1._1, p._1._2, p._2)

    def unbiasR[A, B, C](p: (A, (B, C))): (A, B, C) = (p._1, p._2._1, p._2._2)

    def labelLaw[A](p: Parser[A], inputs: Gen[String]): Prop =
      forAll(inputs ** Gen.stringGen(5)) { case (input, msg) =>
        run(label(msg)(p))(input) match {
          case Left(e) => errorMessage(e) == msg
          case _ => true
        }
      }

    // Description of scope in book (page 163) is if run(p)(s) is Left(e1), then run(scope(msg)(p)) is Left(e2),
    // where e2.stack.head will be msg and e2.stack.tail is e1.
    // NOTE: This is incorrect, see law below.
    def scopeLaw[A](p: Parser[A], inputs: Gen[String]): Prop =
      forAll(inputs ** Gen.stringGen(5) ** Gen.stringGen(5)) { case ((input, msg1), msg2) =>
        run(scope(msg2)(label(msg1)(p)))(input) match {
          case Left(e) => e.stack.head._2 == msg2 && e.stack.tail.head._2 == msg1
          case _ => true
        }
      }
  }

}