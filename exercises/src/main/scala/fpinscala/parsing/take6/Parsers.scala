package fpinscala.parsing.take6

import language.higherKinds
import scala.math.Ordered.orderingToOrdered
import scala.util.matching.Regex

// NOTE: Implementing the algebra, exercise 9.12 (not attempting), altering the type of Parser
//       (as per the top of page 167)
case class Location(input: String, offset: Int = 0) extends Ordered[Location] {
  lazy val line: Int = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col: Int = input.slice(0, offset + 1).lastIndexOf('\n') match {
    case -1 => offset + 1
    case lineStart => offset - lineStart
  }

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int): Location =
    copy(offset = offset + n)

  def remainingInput: String = input.substring(offset)

  override def compare(that: Location): Int = (this.input, this.offset) compare (that.input, that.offset)

  /* Returns the line corresponding to this location */
  def currentLine: String =
    if (input.length > 1) input.lines.drop(line-1).next
    else ""

  def columnCaret: String = (" " * (col-1)) + "^"
}

case class ParseError(stack: List[(Location, String)], otherFailures: List[ParseError] = List()) {
  def push(loc: Location, msg: String): ParseError =
    copy(stack = (loc, msg) :: stack)

  def label[A](s: String): ParseError =
    ParseError(latestLoc.map((_, s)).toList)

  def latestLoc: Option[Location] =
    latest map (_._1)

  def latest: Option[(Location, String)] =
    stack.lastOption

  def formatted: String = stack.groupBy(_._1).toSeq.sortBy(_._1).map {
    case (loc, matches) => s"$loc -> ${matches.map(_._2).mkString(";")}"
  }.toList.mkString(", ")

  override def toString: String =
    if (stack.isEmpty) "no error message"
    else {
      val collapsed = collapseStack(stack)
      collapsed.map { case (loc,msg) => formatLoc(loc) + " " + msg }.mkString("\n") +
        (collapsed.lastOption.map("\n\n" + _._1.currentLine).getOrElse("") +
          collapsed.lastOption.map("\n" + _._1.columnCaret).getOrElse(""))
    }

  /* Builds a collapsed version of the given error stack -
   * messages at the same location have their messages merged,
   * separated by semicolons */
  def collapseStack(s: List[(Location,String)]): List[(Location,String)] =
    s.groupBy(_._1).
      mapValues(_.map(_._2).mkString("; ")).
      toList.sortBy(_._1.offset)

  def formatLoc(l: Location): String = l.line + "." + l.col

  // Exercise 9.18
  // Hint: You can add an attribute `otherFailures: List[ParseError]` on `ParseError` itself. This will be a list of
  // parse errors that occurred in other branches of the parser.
  def addFailure(e: ParseError): ParseError =
    this.copy(otherFailures = e :: this.otherFailures)

  // Of course, we have to decide how to print a `ParseError` for human consumption
  // We also can expose combinators for selecting which error(s) get reported in the
  // event that a chain of `a | b | c` fails--we might choose to collect up all the
  // errors for each of the three parsers, or perhaps only show the parser that got
  // the furthest in the input before failing, etc
}

trait Parsers[Parser[+_]] {
  self => // so inner classes may call methods of trait

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  def run[A](p: Parser[A])(loc: Location): Result[A]

  // Primitives
  implicit def string(s: String): Parser[String]

  implicit def regex(r: Regex): Parser[String]

  def succeed[A](a: A): Parser[A]

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

    // Added for testing JSON with this parser....
    def **[B](p2: Parser[B]): Parser[(A, B)] = self.**(p, p2)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def many: Parser[List[A]] = self.many(p)

    def ignoreLeadingWhitespace: Parser[A] = {
      self.ignoreLeadingWhitespace(p)
    }
  }

  // Added for testing JSON with this parser....
  def **[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)]

  def map[A, B](a: Parser[A])(f: A => B): Parser[B]

  def many[A](p: Parser[A]): Parser[List[A]]

  // TODO BUGFIX - Changed it to 0 or more (hmm, relationship between slice and many?)
  //               Also changed token from \\w to \\s
  private val whitespace = "\\s*".r

  private val slicedWhitespace = slice(whitespace)

  def ignoreLeadingWhitespace[A](p: Parser[A]): Parser[A] =
    slicedWhitespace ** p map (_._2)
}