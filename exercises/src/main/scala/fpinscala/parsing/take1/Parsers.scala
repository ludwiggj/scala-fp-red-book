package fpinscala.parsing.take1

import language.higherKinds

trait Parsers[ParseError, Parser[+_]] {
  self => // so inner classes may call methods of trait

  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

  implicit def int(l: Int): Parser[Int]

  implicit def string(s: String): Parser[String]

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def char(c: Char): Parser[Char]

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
  }

  object Laws {
    // run(char(c))(c.toString) == Right(c)
    // run(string(s))(s) == Right(s)
  }

}

trait ActualParseError

trait ActualParser[+T]

trait Example {
  val v: Parsers[ActualParseError, ActualParser]
  val i1: ActualParser[Int]
  val i2: ActualParser[Int]

  import v._

  def stringParser(): ActualParser[String] = {
    "abra" | "cadabara"
  }

  def intParser1(): ActualParser[Int] = {
    i1 or i2
  }

  def intParser2(): ActualParser[Int] = {
    1.asInstanceOf[ActualParser[Int]] or 2
  }

  def intParser3(): ActualParser[Int] = {
    // This doesn't work!
    // 1 or 2

    int(1) or int(2)
  }

  def listOfNParser(): ActualParser[List[String]] = {
    listOfN(3, "ab" | "cad")
  }

  val stringParseResult: Either[ActualParseError, String] = run(stringParser())("This is a test")

  val intParseResult: Either[ActualParseError, Int] = run(intParser1())("This is a test")

  val stringListParseResult: Either[ActualParseError, List[String]] = run(listOfNParser())("This is a test")
}

object Example {
  def main(args: Array[String]): Unit = {
    println("hi")
  }
}