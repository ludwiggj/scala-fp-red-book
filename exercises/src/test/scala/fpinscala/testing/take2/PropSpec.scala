package fpinscala.testing.take2

import com.typesafe.scalalogging.LazyLogging
import fpinscala.UnitSpec
import fpinscala.testing.take2.Prop.{check, checkOnce, forAll, sortedPrep, sortedPropTextbook}
import fpinscala.testing.take2.SGen.{listOf, listOfL, listOfLAlternative}

class PropSpec extends UnitSpec with LazyLogging {
  val smallInt: Gen[Int] = Gen.choose(-10, 10)

  "list max" should "always return the largest value (listOf)" in {
    val maxProp = forAll(listOf(smallInt)) { ns =>
      println(s"Candidate = $ns")
      if (ns.isEmpty) {
        true
      } else {
        val max = ns.max
        !ns.exists(_ > max)
      }
    }
    assert(Prop.run(maxProp, maxSize = 10, testCases = 30) == Passed)
  }

  it should "always return the largest value (listOfL)" in {
    val maxProp = forAll(listOfL(smallInt)) { ns =>
      println(s"Candidate = $ns")
      !ns.exists(_ > ns.max)
    }
    assert(Prop.run(maxProp, maxSize = 10, testCases = 30) == Passed)
  }

  it should "always return the largest value (listOfLAlternative)" in {
    val maxProp = forAll(listOfLAlternative(smallInt)) { ns =>
      println(s"Candidate = $ns")
      !ns.exists(_ > ns.max)
    }
    assert(Prop.run(maxProp, maxSize = 10, testCases = 30) == Passed)
  }

  "sorted" should "always return elements in ascending order" in {
    assert(Prop.run(sortedPrep(smallInt), maxSize = 10, testCases = 30) == Passed)
  }

  it should "always return elements in ascending order according to textbook prop" in {
    assert(Prop.run(sortedPropTextbook(smallInt), maxSize = 10, testCases = 30) == Passed)
  }

  "check" should "pointlessly recheck multiple times" in {
    assert(Prop.run(check(true), maxSize = 10, testCases = 30) == Passed)
  }

  "checkOnce" should "check property once only" in {
    // But still prints message suggesting it's been tested multiple times
    assert(Prop.run(checkOnce(true), maxSize = 10, testCases = 30) == Passed)
  }
}