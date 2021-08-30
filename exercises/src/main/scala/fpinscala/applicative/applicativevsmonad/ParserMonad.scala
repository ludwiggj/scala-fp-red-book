package fpinscala.applicative.applicativevsmonad

import fpinscala.applicative.Monad

import java.util.Date

object ParserMonad {
  //noinspection NotImplementedCode
  object MonadExample {
    val F: Monad[Parser] = ???
    val d: Parser[Date] = ???
    val temp: Parser[Double] = ???

    val header: Parser[Parser[Row]] = ???

    val rows: Parser[List[Row]] = F.flatMap(header) { row => row.sep("\n") }
  }
}