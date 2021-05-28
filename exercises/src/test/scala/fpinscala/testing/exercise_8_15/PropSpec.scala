package fpinscala.testing.exercise_8_15

import com.typesafe.scalalogging.LazyLogging
import fpinscala.UnitSpec
import fpinscala.testing.exercise_8_15.Prop.forAll
import fpinscala.testing.take2.Gen
import fpinscala.testing.take2.SGen.listOfLAlternative

class PropSpec extends UnitSpec with LazyLogging {
  val smallInt: Gen[Int] = Gen.choose(-10, 10)

  "list max" should "always return the largest value (listOfLAlternative)" in {
    val maxPropTrue: Prop = forAll(listOfLAlternative(smallInt)) { ns =>
      !ns.exists(_ > ns.max)
    }
    assert(Prop.run(maxPropTrue, maxSize = 10, testCases = 30) == Passed)
  }

  "it" should "never return a value smaller than the largest value" in {
    val maxPropFalse: Prop = forAll(listOfLAlternative(smallInt)) { ns =>
      !ns.exists(_ >= ns.max)
    }
    assert(Prop.run(maxPropFalse, maxSize = 10, testCases = 30) == Falsified)
  }
}