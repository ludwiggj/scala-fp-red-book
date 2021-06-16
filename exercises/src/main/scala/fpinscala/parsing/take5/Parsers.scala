package fpinscala.parsing.take5

import language.higherKinds
import scala.util.matching.Regex

// NOTE: Implementing the algebra, exercise 9.12 (not attempting)
case class Location(input: String, offset: Int = 0) {
  private lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
  private lazy val col = input.slice(0, offset + 1).lastIndexOf('\n') match {
    case -1 => offset + 1
    case lineStart => offset - lineStart
  }

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))
}

case class ParseError(stack: List[(Location, String)])

trait Parsers[Parser[+_]] {
  self => // so inner classes may call methods of trait

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  // Primitives
  implicit def string(s: String): Parser[String]

  implicit def regex(r: Regex): Parser[String]

  def slice[A](p: Parser[A]): Parser[String]

  // Assign error message to a parser
  // If p fails, its ParseError will somehow incorporate msg
  // It will throw away any existing labels attached to the parser (see section 9.5.2, pg 162)
  def label[A](msg: String)(p: Parser[A]): Parser[A]

  // This is a way to nest labels. This method doesn't throw away labels attached to p
  // It merely adds additional information in the event that p fails
  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  // Attempt delays committing to a parse
  def attempt[A](p: Parser[A]): Parser[A]

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def slice: Parser[String] = self.slice(p)

    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
  }
}