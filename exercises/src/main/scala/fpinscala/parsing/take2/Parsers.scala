package fpinscala.parsing.take2

import fpinscala.testing.take2.Prop.forAll
import fpinscala.testing.take2.{Gen, Prop}

import language.higherKinds
import scala.util.matching.Regex

// NOTE: Textbook algebra, page 152 onwards
trait Parsers[ParseError, Parser[+_]] {
  self => // so inner classes may call methods of trait

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)

  implicit def string(s: String): Parser[String]

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  implicit def char(c: Char): Parser[Char] =
    string(c.toString) map (_.charAt(0))

  implicit def asCharParser[A](a: A)(implicit f: A => Parser[Char]): ParserOps[Char] = ParserOps(f(a))

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  // 9.2, a possible algebra
  def many[A](p: Parser[A]): Parser[List[A]]

  def map[A, B](a: Parser[A])(f: A => B): Parser[B]

  def succeed[A](a: A): Parser[A] =
    string("") map (_ => a)

  def slice[A](p: Parser[A]): Parser[String]

  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)]

  // Exercise 9.1 - Implement map2 using product
  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    p ** p2 map { case (a, b) => f(a, b) }

  // Textbook version
  def map2Textbook[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    map(product(p, p2))(f.tupled)

  // Implement many1 in terms of many, using map2
  def many1[A](p: Parser[A]): Parser[List[A]] = {
    map2(p, many(p))((a, la) => a +: la)
  }

  // Textbook version
  def many1Textbook[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _)

  // Implement parser for zero or more 'a', followed by one or more 'b'
  val p: Parser[(Int, Int)] = 'a'.many.slice.map(_.length) ** 'b'.many1.slice.map(_.length)

  // Exercise 9.3 - define many in terms of or, map2 and succeed
  def manyViaOtherFunctions[A](p: Parser[A]): Parser[List[A]] = {
    // def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]
    // def map2[A, B, C](p: Parser[A], p2: Parser[B])(f: (A, B) => C): Parser[C]
    // def succeed[A](a: A): Parser[A]
    map2(p, manyViaOtherFunctions(p))(_ :: _) or succeed(List())
  }

  // Exercise 9.4 - implement listOfN combinator using map2 and succeed
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = {
    if (n <= 0)
      succeed(List())
    else
      map2(p, listOfN(n - 1, p))(_ :: _)
  }

  // Exercise 9.5 - we could also deal with non-strictness with a separate combinator

  // Textbook answer

  // We could introduce a combinator, `wrap`:
  //
  //  def wrap[A](p: => Parser[A]): Parser[A]
  //
  // Then define `many` as:
  //
  //  def many[A](p: Parser[A]): Parser[List[A]] =
  //    map2(p, wrap(many(p)))(_ :: _) or succeed(List())
  //
  // In the parallelism chapter, we were particularly interested in avoiding having `Par` objects that
  // took as much time and space to build as the corresponding serial computation, and the `delay`
  // combinator let us control this more carefully. Here, this isn't as much of a concern, and having
  // to think carefully each time we `map2` to decide whether we need to call `wrap` seems like
  // unnecessary friction for users of the API.

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    // 9.2, a possible algebra
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

    def succeedLaw[A](a: A)(in: Gen[String]): Prop = {
      forAll(in)(s => run(succeed(a))(s) == Right(a))
    }

    // Exercise 9.2
    def productLaw[A, B](a: A, p2: Parser[B])(in: Gen[String]): Prop = {
      val p1 = succeed(a)
      forAll(in)(s => {
        val result1 = run(product(p1, p2))(s)
        val result2 = run(product(p2, p1))(s)

        (result1, result2) match {
          case (Left(e1), Left(e2)) => e1 == e2
          case (Right((a1, b1)), Right((b2, a2))) => (a1 == a2) && (b1 == b2)
          case _ => false
        }
      })
    }

    // Textbook hint

    // Multiplication of numbers is associative, `a * (b * c) == (a * b) * c`.
    // Is there an analogous property for parsers?
    // What can you say about the relationship between `map` and `product`?

    /*
    def productLaw2[A, B, C](pa: Parser[A], pb: Parser[B], pc: Parser[C])(in: Gen[String]): Prop = {
      val p_a_bc: Parser[(A, (B, C))] = pa ** (pb ** pc)
      val p_ab_c: Parser[((A, B), C)] = (pa ** pb) ** pc

      forAll(in)(s => {
        val result1: Either[ParseError, (A, (B, C))] = run(p_a_bc)(s)
        val result2: Either[ParseError, ((A, B), C)] = run(p_ab_c)(s)
        ...
      }
    }

    // map, could map Parser[A] to succeed
    */

    // Textbook answer

    // `product` is associative. These two expressions are "roughly" equal:

    // (a ** b) ** c
    // a ** (b ** c)

    // The only difference is how the pairs are nested. The `(a ** b) ** c` parser returns an `((A,B), C)`,
    // whereas the `a ** (b ** c)` returns an `(A, (B,C))`. We can define functions `unbiasL` and `unbiasR`
    // to convert these nested tuples to flat 3-tuples:

    def unbiasL[A, B, C](p: ((A, B), C)): (A, B, C) = (p._1._1, p._1._2, p._2)

    def unbiasR[A, B, C](p: (A, (B, C))): (A, B, C) = (p._1, p._2._1, p._2._2)

    // With these, we can now state the associativity property:
    // (a ** b) ** c map (unbiasL) == a ** (b ** c) map (unbiasR)

    // We'll sometimes just use `~=` when there is an obvious bijection between the two sides:

    // (a ** b) ** c ~= a ** (b ** c)

    // `map` and `product` also have an interesting relationship -- we can `map` either before or after taking
    // the product of two parsers, without affecting the behavior:

    // a.map(f) ** b.map(g) == (a ** b) map { case (a,b) => (f(a), g(b)) }

    // For instance, if `a` and `b` were both `Parser[String]`, and `f` and `g` both computed the length of a
    // string, it doesn't matter if we map over the result of `a` to compute its length, or whether we do that
    // _after_ the product.

    val numA: Parser[Int] = char('a').many.map(_.size)

    val result: Either[ParseError, List[Char]] = run(('a' or 'b').many)("a test string")
    val count: Either[ParseError, Int] = run(('a' or 'b').many.map(_.length))("a test string")

    val result2: Either[ParseError, String] = run(('a' or 'b').many.slice)("a test string")
    val count2: Either[ParseError, Int] = run(('a' or 'b').many.slice.map(_.length))("a test string")
  }

  // Exercise 9.6
  // Using flatMap and any other combinators, write the context-sensitive parser we couldn't express earlier.

  // Suppose we want to parse a single digit, like '4', followed by that many 'a' characters
  // Examples of valid input are "0", "1a", "2aa", "4aaaa", and so on. This is an example of
  // a context-sensitive grammar. It can’t be expressed with product because our choice of
  // the second parser depends on the result of the first (the second parser depends on its
  // context). We want to run the first parser, and then do a listOfN using the number
  // extracted from the first parser’s result.

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  implicit def regex(r: Regex): Parser[String]

  val parser: Parser[List[Char]] = "\\d".r flatMap (s => listOfN(s.toInt, 'a'))

  // Textbook answer
  // We'll just have this parser return the number of `"a"` characters read. Note that we can
  // declare a normal value inside a for-comprehension.
  val result: Parser[Int] = for {
    digit <- "[0-9]+".r
    n = digit.toInt // we really should catch exceptions thrown by toInt and convert to parse failure
    _ <- listOfN(n, char('a'))
  } yield n

  // Exercise 9.7
  // Implement product and map2 in terms of flatMap.
  def productImp[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    for {
      a <- p
      b <- p2
    } yield (a, b)

  def map2Imp[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    for {
      a <- p
      b <- p2
    } yield f(a, b)

  // These can be implemented using a for-comprehension, (as per above) which delegates to flatMap and map
  // or they can be implemented in terms of these functions directly.

  def productImp2[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    flatMap(p)(a => map(p2)(b => (a, b)))

  // Exercise 9.8
  // map is no longer primitive. Express it in terms of flatMap and/or other combinators.

  def mapImp[A, B](p: Parser[A])(f: A => B): Parser[B] =
    p flatMap (a => succeed(f(a)))
}