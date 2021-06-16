package fpinscala.parsing.take3

import language.{higherKinds, postfixOps}
import scala.util.matching.Regex

trait Parsers[ParseError, Parser[+_]] {
  self => // so inner classes may call methods of trait

  // Primitives - start
  implicit def string(s: String): Parser[String]

  implicit def regex(r: Regex): Parser[String]

  def slice[A](p: Parser[A]): Parser[String]

  def succeed[A](a: A): Parser[A]

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  // Primitives - end

  // Implicits
  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  // Run
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  // Additional operators
  def **[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)]

  def map[A, B](a: Parser[A])(f: A => B): Parser[B]

  def many[A](p: Parser[A]): Parser[List[A]]

  private val whitespace = "\\w".r

  private val slicedWhitespace = slice(whitespace)

  def ignoreLeadingWhitespace[A](p: Parser[A]): Parser[A] =
    slicedWhitespace ** p map (_._2)

  // ParserOps
  case class ParserOps[A](p: Parser[A]) {
    // Primitives - start
    def slice: Parser[String] = self.slice(p)

    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)

    def |[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    // Primitives - end

    // Additional operators
    def **[B](p2: Parser[B]): Parser[(A, B)] = self.**(p, p2)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def many: Parser[List[A]] = self.many(p)

    def ignoreLeadingWhitespace: Parser[A] = {
      self.ignoreLeadingWhitespace(p)
    }
  }
}