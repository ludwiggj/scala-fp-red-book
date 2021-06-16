package fpinscala.parsing.take1

import language.higherKinds
import scala.annotation.tailrec

// NOTE: Attempt at coming up with a possible algebra (page 151)

trait ParseErrorAPI {
  def msg: String

  // Can add other methods here as needed i.e. choose functions for the API of the parser
}

trait ParserAPI[+A] {
  def target: A
}

trait Parsers[ParseError <: ParseErrorAPI, Parser[+_] <: ParserAPI[_]] {
  self => // so inner classes may call methods of trait

  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)

  implicit def string(s: String): Parser[String]

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  implicit def int(l: Int): Parser[Int]

  implicit def asIntParser[A](a: A)(implicit f: A => Parser[Int]): ParserOps[Int] = ParserOps(f(a))

  type Runnable[A, B] = Parser[A] => String => Either[ParseError, B]

  // Add something in here to control what errors are supported
  type RunnableWithErrorFilter[A, B] = (Parser[A], ParseError => Boolean) => String => Either[ParseError, B]

  def test[A]: RunnableWithErrorFilter[A, A] = ???

  val result: Either[ParseError, String] = test[String]("bob", _.msg.contains("report this"))("freddie")

  type Parsable[A] = String => Either[ParseError, A]

  def run[A]: Runnable[A, A]

  def char(c: Char): Parser[Char]

  def atLeast[A](n: Int): Runnable[A, Int] = {
    parser =>
      s1 => {
        @tailrec
        def parse(count: Int)(s: String): Either[ParseError, Int] = {
          run(parser)(s) match {
            case Right(_) => parse(count + 1)(s.tail)
            // case _: Left[ParseError, Int] => if (count >= n) Right(count) else Left(
            // This does not compile, as ParseError is not a class!
            //   new ParseError {
            //     override def msg: String = s"Expected $count or more '${parser.target}'"
            //   }
            // )
          }
        }

        parse(0)(s1)
      }
  }

  def combine(p1: Parsable[Int], p2: Parsable[Int]): Parsable[(Int, Int)] = {
    s =>
      (p1(s), p2(s)) match {
        case (Right(x), Right(y)) => Right(x, y)
        case (Right(_), Left(pe)) => Left(pe)
        case (Left(pe), Right(_)) => Left(pe)
          // case (Left(pe1), Left(pe2)) => Left(
          // This does not compile, as ParseError is not a class!
          // new ParseError {
          //   override def msg: String = s"${pe1.msg}\n${pe2.msg}"
          // }
          // )
      }
  }

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
  }

}

trait ActualParseError extends ParseErrorAPI

trait ActualParser[+T] extends ParserAPI[T]

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
    1 or 2
  }

  def listOfNParser(): ActualParser[List[String]] = {
    listOfN(3, "ab" | "cad")
  }

  val stringParseResult: Either[ActualParseError, String] = run(stringParser())("This is a test")

  val intParseResult: Either[ActualParseError, Int] = run(intParser1())("This is a test")

  val stringListParseResult: Either[ActualParseError, List[String]] = run(listOfNParser())("This is a test")

  // Parser[Int], recognising 0 or more 'a' characters
  val zeroOrMoreA: Parsable[Int] = atLeast[Char](0)(char('a'))

  // Parser[Int], recognising 1 or more 'a' characters
  val oneOrMoreB: Parsable[Int] = atLeast[Char](1)(char('b'))

  val combined: Parsable[(Int, Int)] = combine(zeroOrMoreA, oneOrMoreB)
}

object Example {
  def main(args: Array[String]): Unit = {
    println("hi")
  }

  // Question: Does "a | b" mean the same as "b | a"?

  // If yes:
  //   - Referentially transparent
  //   - How do we determine which one matches first, if they both match? (not sure how to solve this)

  // If no:
  //   - Not referentially transparent
  //   - Easier to implement, just choose first one that matches

  // Question: Does "a | (b | c)" mean the same as "(a | b) | c"?
  //           If yes, is this a primitive law, or is it implied by something simpler?

  // ?

  // Question: Laws?

  // ?
}