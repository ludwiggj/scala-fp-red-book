package fpinscala.parsing.take6

import fpinscala.parsing.take6.MyParser.Parser

import scala.annotation.tailrec
import scala.util.matching.Regex

object MyParser {
  type Parser[+A] = Location => Result[A]
}

trait Result[+A] {
  def mapError(f: ParseError => ParseError): Result[A] = this match {
    case Failure(e, isCommitted) => Failure(f(e), isCommitted)
    case _ => this
  }

  def uncommit: Result[A] = this match {
    case Failure(e, true) => Failure(e, isCommitted = false)
    case _ => this
  }

  def addCommit(isCommitted: Boolean): Result[A] = this match {
    case Failure(e, c) => Failure(e, c || isCommitted)
    case _ => this
  }

  def advanceSuccess(n: Int): Result[A] = this match {
    case Success(a, m) => Success(a, n + m)
    case _ => this
  }
}

case class Success[+A](get: A, charsConsumed: Int) extends Result[A] {
  override def toString: String = s"Success(\n  get: $get\n  charsConsumed:$charsConsumed\n)"
}

case class Failure(get: ParseError, isCommitted: Boolean) extends Result[Nothing]

object Parser extends Parsers[Parser] {
  // Exercise 9.13
  override implicit def string(s: String): Parser[String] =
    loc =>
      if (loc.remainingInput.startsWith(s))
        Success(s, s.length)
      else
        Failure(
          Location(loc.input).toError(
            s"Expected string: [$s] Offset [${loc.offset}] Remaining [${loc.remainingInput}]"),
          isCommitted = true
        )

  override implicit def regex(r: Regex): Parser[String] =
    loc =>
      r.findFirstMatchIn(loc.remainingInput).map(m => (m.start, m.matched)) match {
        case Some((0, m)) =>
          Success(m, m.length)
        case _ =>
          Failure(
            Location(loc.input).toError(
              s"Expected regex: [$r] Offset [${loc.offset}] Remaining [${loc.remainingInput}]"
            ),
            isCommitted = true
          )
      }

  def regexTextbook(r: Regex): Parser[String] =
    loc =>
      // findPrefixOf returns an optional match of this Regex at the beginning of the given character sequence,
      // or None if it matches no prefix of the character sequence. Unlike findFirstIn, this method will only
      // return a match at the beginning of the input.
      r.findPrefixOf(loc.input) match {
        case Some(m) => Success(m, m.length)
        case _ => Failure(Location(loc.input).toError("Expected: " + r), isCommitted = true)
      }

  override def succeed[A](a: A): Parser[A] =
    _ => Success(a, 0)

  override def slice[A](p: Parser[A]): Parser[String] =
    loc =>
      p(loc) match {
        case Success(_, n) =>
          Success(loc.input.substring(loc.offset, loc.offset + n), n)
        case f: Failure =>
          f
      }

  override def label[A](msg: String)(p: Parser[A]): Parser[A] =
    loc => p(loc).mapError(_.label(msg))

  override def scope[A](msg: String)(p: Parser[A]): Parser[A] =
    loc => p(loc).mapError(_.push(loc, msg))

  // Exercise 9.14 - revise implementation of string, to use scope and/or label to provide a meaningful
  //                 error message in the event of an error

  // Hint: You may want `string` to report the immediate cause of failure (whichever character didn't match),
  // as well as the overall string being parsed.

  // TODO - Solution for Exercise 9.14 not provided, as referenced code does not use label nor scope
  //        Requirement to report immediate cause of failure suggests a more detailed implementation
  //        than startsWith
  def stringRevised(s: String): Parser[String] =
    loc =>
      if (loc.input.startsWith(s))
        Success(s, s.length)
      else
        Failure(Location(loc.input).toError("Expected: " + s), isCommitted = true)

  override def flatMap[A, B](f: Parser[A])(g: A => Parser[B]): Parser[B] =
    loc => f(loc) match {
      // (1) Advance the source location BEFORE calling the second parser
      // (2) Commit if the first parser has consumed any characters
      // (3) If successful, we increment the number of characters consumed by n, to account for characters already consumed by f
      case Success(a, n) => g(a)(loc.advanceBy(n)).addCommit(n != 0).advanceSuccess(n)
      case e@Failure(_, _) => e
    }

  override def attempt[A](p: Parser[A]): Parser[A] =
    loc => p(loc).uncommit

  override def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A] =
    loc => s1(loc) match {
      case Failure(e, false) =>
        s2(loc).mapError(_.addFailure(e))
      // TODO BUG WORKAROUND - backtracking (which should reset isCommitted to false) is not working,
      //                       so just try the other parser in this case
      case Failure(e, true) =>
        s2(loc).mapError(_.addFailure(e))
      // This matches on Success
      case r =>
        r
    }

  def or[A](p: Parser[A], p2: => Parser[A]): Parser[A] =
    s => p(s) match {
      case Failure(e,false) => p2(s).mapError(_.addFailure(e))
      case r => r // committed failure or success skips running `p2`
    }

  // Exercise 9.15 - implement the rest of the primitives, including run (this is the only one left :)
  override def run[A](p: Parser[A])(loc: Location): Result[A] = p(loc)

  override def **[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    flatMap(p)(a => map(p2)(b => (a, b)))

  override def map[A, B](p: Parser[A])(f: A => B): Parser[B] =
    p flatMap (a => succeed(f(a)))

  // Textbook...
  // We provide an overridden version of `many` that accumulates
  // the list of results using a monolithic loop. This avoids
  // stack overflow errors for most grammars.
  override def many[A](p: Parser[A]): Parser[List[A]] =
    loc => {
      val buf = new collection.mutable.ListBuffer[A]

      @tailrec
      def go(p: Parser[A], offset: Int): Result[List[A]] = {
        p(loc.advanceBy(offset)) match {
          case Success(a, n) => buf += a; go(p, offset + n)
          // TODO Not sure why there are two different failure cases here?
          //      Note that this actually caused the failure of the many clause...
          // case f@Failure(_, true) => f
          case Failure(_, _) => Success(buf.toList, offset) // Terminal case
        }
      }

      go(p, offset = 0)
    }

  // TODO - Exercise 9.17 (optional)
}