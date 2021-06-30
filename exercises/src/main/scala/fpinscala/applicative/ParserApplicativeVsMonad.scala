package fpinscala.applicative

import fpinscala.parsing.take6.{Location, Result}

import java.util.Date

//noinspection NotImplementedCode,ScalaUnusedSymbol
class Parser[A] {
  def sep(str: String): Parser[List[A]] = ???
  def parse(loc: Location): Result[A] = ???
}

object ParserApplicativeVsMonad {
  case class Row(date: Date, temperature: Double)

  //noinspection NotImplementedCode
  object ApplicativeExample {
    val F: Applicative[Parser] = ???
    val d: Parser[Date] = ???
    val temp: Parser[Double] = ???

    val row: Parser[Row] = F.map2(d, temp)(Row)
    val rows: Parser[List[Row]] = row.sep("\n")
  }

  //noinspection NotImplementedCode
  object MonadExample {
    val F: Monad[Parser] = ???
    val d: Parser[Date] = ???
    val temp: Parser[Double] = ???

    val header: Parser[Parser[Row]] = ???

    val rows: Parser[List[Row]] = F.flatMap(header) { row => row.sep("\n") }
  }
}
