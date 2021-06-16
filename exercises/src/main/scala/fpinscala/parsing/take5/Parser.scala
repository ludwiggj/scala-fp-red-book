package fpinscala.parsing.take5

import fpinscala.parsing.take5.MyParser.Parser

import scala.util.matching.Regex

object MyParser {
  type Parser[+A] = String => Either[ParseError, A]
}

object Parser extends Parsers[Parser] {
  override def run[A](p: Parser[A])(input: String): Either[ParseError, A] = ???

  override implicit def string(s: String): Parser[String] =
    (input: String) =>
      if (input.startsWith(s))
        Right(s)
      else
        Left(Location(input).toError("Expected: " + s))

  override implicit def regex(r: Regex): Parser[String] = ???

  override def slice[A](p: Parser[A]): Parser[String] = ???

  override def label[A](msg: String)(p: Parser[A]): Parser[A] = ???

  override def scope[A](msg: String)(p: Parser[A]): Parser[A] = ???

  override def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] = ???

  override def attempt[A](p: Parser[A]): Parser[A] = ???

  override def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A] = ???
}
