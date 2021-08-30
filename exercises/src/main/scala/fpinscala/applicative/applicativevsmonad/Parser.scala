package fpinscala.applicative.applicativevsmonad

import fpinscala.parsing.take6.{Location, Result}

import java.util.Date

//noinspection NotImplementedCode,ScalaUnusedSymbol
class Parser[A] {
  def sep(str: String): Parser[List[A]] = ???
  def parse(loc: Location): Result[A] = ???
}

case class Row(date: Date, temperature: Double)