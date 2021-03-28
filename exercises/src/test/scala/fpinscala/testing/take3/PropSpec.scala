package fpinscala.testing.take3

import com.typesafe.scalalogging.LazyLogging
import fpinscala.UnitSpec
import fpinscala.testing.take2.Gen
import fpinscala.testing.take2.SGen.{listOf, listOfL, listOfLAlternative}
import fpinscala.testing.take3.Prop._

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

  "check" should "return proved if it passes" in {
    assert(Prop.run(check(true), maxSize = 10, testCases = 30) == Proved)
  }

  it should "return falsified if it fails" in {
    assert(Prop.run(check(false), maxSize = 10, testCases = 30) match {
      case Falsified(_, _) => true
      case _ => false
    })
  }

  "properties combined with ampersand" should "pass if both succeed and second property is not a check" in {
    assert(Prop.run(check(true).&&(maxPropTrue), maxSize = 10, testCases = 30) == Passed)
  }

  it should "be proven if both succeed and second property is a check" in {
    assert(Prop.run(maxPropTrue.&&(check(true)), maxSize = 10, testCases = 30) == Proved)
    assert(Prop.run(check(true).&&(check(true)), maxSize = 10, testCases = 30) == Proved)
  }

  it should "fail if either property fails" in {
    assert(Prop.run(check(false).&&(check(true)), maxSize = 10, testCases = 30) match {
      case Falsified(_, _) => true
      case _ => false
    })

    assert(Prop.run(maxPropFalse.&&(check(true)), maxSize = 10, testCases = 30) match {
      case Falsified(_, _) => true
      case _ => false
    })
  }

  "properties combined with or" should "pass if either succeed and first pass is not a check" in {
    assert(Prop.run(maxPropTrue.||(maxPropFalse), maxSize = 10, testCases = 30) == Passed)
  }

  it should "pass if one succeeds and first pass is a check" in {
    assert(Prop.run(maxPropFalse.||(check(true)), maxSize = 10, testCases = 30) == Proved)
  }

  it should "fail if both fail" in {
    assert(Prop.run(check(false).&&(maxPropFalse), maxSize = 10, testCases = 30) match {
      case Falsified(_, _) => true
      case _ => false
    })
  }

  "parMap" should "work" in {
    assert(Prop.run(parProp, maxSize = 10, testCases = 30) == Passed)
    assert(Prop.run(parProp2, maxSize = 10, testCases = 30) == Proved)
    assert(Prop.run(parProp3, maxSize = 10, testCases = 30) == Proved)
    assert(Prop.run(parProp4, maxSize = 10, testCases = 30) == Passed)
  }

  "par map law 1" should "hold" in {
    assert(Prop.run(parMapLaw1, maxSize = 10, testCases = 30) == Passed)
  }

  "par map law 2" should "hold" in {
    assert(Prop.run(parMapLaw2, maxSize = 10, testCases = 30) == Passed)
  }

  "par fork law 1" should "hold" in {
    assert(Prop.run(parForkLaw1, maxSize = 10, testCases = 30) == Passed)
  }

  "par fork law 2" should "hold" in {
    assert(Prop.run(parForkLaw2, maxSize = 10, testCases = 30) == Passed)
  }

  "takeWhile property" should "hold" in {
    assert(Prop.run(takeWhileProp, maxSize = 10, testCases = 30) == Passed)
    assert(Prop.run(takeWhileProp2, maxSize = 10, testCases = 30) == Passed)
    assert(Prop.run(takeWhileProp3, maxSize = 10, testCases = 30) == Passed)
  }
}