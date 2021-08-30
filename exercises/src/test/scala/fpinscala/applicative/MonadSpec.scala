package fpinscala.applicative

import fpinscala.UnitSpec
import scala.language.higherKinds

class MonadSpec extends UnitSpec {
  // Exercise 12.5
  "eitherMonad" can "return valid result" in {
    val result = for {
      a <- Monads.eitherMonad[String].unit(2)
      b <- Monads.eitherMonad[String].unit(22)
    } yield a + b
    assert(result == Right(24))
  }

  it can "return failure" in {
    val result = for {
      a <- Monads.eitherMonad[String].unit(2)
      b <- Monads.eitherMonad[String].error(Left[String, Int]("Oh dear"))
    } yield a + b
    assert(result.left.get == "Oh dear")
  }
}