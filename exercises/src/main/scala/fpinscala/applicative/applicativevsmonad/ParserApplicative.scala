package fpinscala.applicative.applicativevsmonad

import fpinscala.applicative.Applicative

import java.util.Date

object ParserApplicative {
  //noinspection NotImplementedCode
  object ApplicativeExample {
    val F: Applicative[Parser] = ???
    val d: Parser[Date] = ???
    val temp: Parser[Double] = ???

    val row: Parser[Row] = F.map2(d, temp)(Row)
    val rows: Parser[List[Row]] = row.sep("\n")
  }
}