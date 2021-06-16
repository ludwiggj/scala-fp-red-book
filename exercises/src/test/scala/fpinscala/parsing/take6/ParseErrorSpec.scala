package fpinscala.parsing.take6

import com.typesafe.scalalogging.LazyLogging
import fpinscala.UnitSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class ParseErrorSpec extends UnitSpec with ScalaCheckPropertyChecks with LazyLogging {

  private val usTwo =
    """Wherever I am, there's always Pooh,
      |There's always Pooh and Me.
      |Whatever I do, he wants to do,
      |"Where are you going today?" says Pooh:
      |"Well, that's very odd 'cos I was too.
      |Let's go together," says Pooh, says he.
      |"Let's go together," says Pooh.""".stripMargin

  private val parseError = ParseError(List(
    (Location(usTwo, 25), "Message 1"),
    (Location(usTwo, 30), "Message 2"),
    (Location(usTwo, 25), "Message 3"),
    (Location(usTwo, 25), "Message 4"),
    (Location(usTwo, 45), "Message 5")
  ))

  // Exercise 9.16
  "parseError" should "format errors" in {
    val expectedMessage =
      s"Location($usTwo,25) -> Message 1;Message 3;Message 4," +
        s" Location($usTwo,30) -> Message 2," +
        s" Location($usTwo,45) -> Message 5"

    assert(parseError.formatted == expectedMessage)
  }

  it should "format errors using textbook solution" in {
    val expectedMessage =
      "1.26 Message 1; Message 3; Message 4\n" +
        "1.31 Message 2\n" +
        "2.9 Message 5\n\n" +
        "There's always Pooh and Me.\n" +
        "        ^"

    assert(parseError.toString == expectedMessage)
  }
}