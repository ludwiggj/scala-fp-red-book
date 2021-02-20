package fpinscala.state

import com.typesafe.scalalogging.LazyLogging
import fpinscala.UnitSpec
import fpinscala.state.CandyMachine.{Coin, Machine, Turn, simulateMachine, simulateMachine2}
import fpinscala.state.RNG._
import fpinscala.state.State.{numbers1, numbers2}
import org.scalactic.anyvals.PosInt
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class StateSpec extends UnitSpec with ScalaCheckPropertyChecks with LazyLogging {
  private val numberOfPropertyBasedTests = PosInt.from(100).get

  private def isPositiveInteger(i: Int): Unit = {
    logger.debug(s"Result is $i")

    assert(i >= 0)
    assert(i <= Integer.MAX_VALUE)
  }

  private def isDoubleBetween0InclusiveAnd1Exclusive(d: Double): Unit = {
    logger.debug(s"Result is $d")

    assert(d >= 0)
    assert(d < 1)
  }

  private def isIntegerBetweenXInclusiveAndYExclusive(d: Int, x: Int, y: Int): Unit = {
    logger.debug(s"Result is $d")

    assert(d >= x)
    assert(d < y)
  }

  // Exercise 6.1
  "nonNegativeInt" should "return an integer" in {
    forAll(minSuccessful(numberOfPropertyBasedTests)) { seed: Long =>
      val simple = Simple(seed)

      isPositiveInteger(nonNegativeInt(simple)._1)
      isPositiveInteger(nonNegativeIntTextbook(simple)._1)
    }
  }

  // Exercise 6.2
  "double" should "return a double between 0 inclusive and 1 exclusive" in {
    forAll(minSuccessful(numberOfPropertyBasedTests)) { seed: Long =>
      val simple = Simple(seed)

      isDoubleBetween0InclusiveAnd1Exclusive(double(simple)._1)
      isDoubleBetween0InclusiveAnd1Exclusive(doubleTextbook(simple)._1)

      // Exercise 6.5
      isDoubleBetween0InclusiveAnd1Exclusive(doubleViaMap(simple)._1)
      isDoubleBetween0InclusiveAnd1Exclusive(doubleViaMapTextbook(simple)._1)
    }
  }

  // Exercise 6.3
  "intDouble" should "return an int double tuple" in {
    forAll(minSuccessful(numberOfPropertyBasedTests)) { seed: Long =>
      val simple = Simple(seed)

      isDoubleBetween0InclusiveAnd1Exclusive(intDouble(simple)._1._2)

      // Exercise 6.6
      isDoubleBetween0InclusiveAnd1Exclusive(intDoubleViaBoth(simple)._1._2)
    }
  }

  // Exercise 6.3
  "doubleInt" should "return a double int tuple" in {
    forAll(minSuccessful(numberOfPropertyBasedTests)) { seed: Long =>
      val simple = Simple(seed)

      isDoubleBetween0InclusiveAnd1Exclusive(doubleInt(simple)._1._1)
      isDoubleBetween0InclusiveAnd1Exclusive(doubleIntTextbook(simple)._1._1)

      // Exercise 6.6
      isDoubleBetween0InclusiveAnd1Exclusive(doubleIntViaBoth(simple)._1._1)
    }
  }

  // Exercise 6.3
  "double3" should "return a double triplet tuple" in {
    forAll(minSuccessful(numberOfPropertyBasedTests)) { seed: Long =>
      val ((d1, d2, d3), _) = double3(Simple(seed))
      isDoubleBetween0InclusiveAnd1Exclusive(d1)
      isDoubleBetween0InclusiveAnd1Exclusive(d2)
      isDoubleBetween0InclusiveAnd1Exclusive(d3)
    }
  }

  val expectedInts = List(384748, -1151252339, -549383847, 1612966641, -883454042)
  val expectedIntsReverseOrder = List(-883454042, 1612966641, -549383847, -1151252339, 384748)

  // Exercise 6.4
  "ints" should "return list of ints in reverse order" in {
    val simple = Simple(seed = 1)

    assert(ints(5)(simple)._1 == expectedIntsReverseOrder)
    assert(intsTextbookTailRecursive(5)(simple)._1 == expectedIntsReverseOrder)
  }

  it should "return list of ints in order" in {
    assert(intsTextbookNotTailRecursive(5)(Simple(1))._1 == expectedInts)
  }

  // Exercise 6.7
  "sequence" should "sequence computations" in {
    val inputList = List(unit(1), unit(2), unit(3))
    val expectedList = List(1, 2, 3)

    val simple = Simple(seed = 1)

    assert(sequenceFoldLeft(inputList)(simple)._1 == expectedList)
    assert(sequenceTextbookFoldLeft(inputList)(simple)._1 == expectedList)
    assert(sequenceFoldRight(inputList)(simple)._1 == expectedList)
    assert(sequenceTextbookFoldRight(inputList)(simple)._1 == expectedList)

    // Exercise 6.9
    assert(sequenceTextbookFoldLeftViaMapViaFlatMap(inputList)(simple)._1 == expectedList)
    assert(sequenceTextbookFoldLeftViaMapViaFlatMap2(inputList)(simple)._1 == expectedList)
    assert(sequenceTextbookFoldRightViaMap2ViaFlatMap(inputList)(simple)._1 == expectedList)
    assert(sequenceTextbookFoldRightViaMap2ViaFlatMapTextbook(inputList)(simple)._1 == expectedList)
  }

  "intsViaSequenceFoldRight" should "return list of ints in order BUT doesn't" in {
    assert(intsViaSequence(sequenceFoldRight)(5)(Simple(1))._1 == expectedInts)
  }

  "intsViaSequence" should "return list of ints in order" in {
    val simple = Simple(seed = 1)

    assert(intsViaSequence(sequenceFoldLeft)(5)(simple)._1 == expectedInts)
    assert(intsViaSequence(sequenceTextbookFoldRight)(5)(simple)._1 == expectedInts)
    assert(intsViaSequence(sequenceTextbookFoldLeft)(5)(simple)._1 == expectedInts)

    // Exercise 6.9
    assert(intsViaSequence(sequenceTextbookFoldLeftViaMapViaFlatMap)(5)(simple)._1 == expectedInts)
    assert(intsViaSequence(sequenceTextbookFoldLeftViaMapViaFlatMap2)(5)(simple)._1 == expectedInts)
    assert(intsViaSequence(sequenceTextbookFoldRightViaMap2ViaFlatMap)(5)(simple)._1 == expectedInts)
    assert(intsViaSequence(sequenceTextbookFoldRightViaMap2ViaFlatMapTextbook)(5)(simple)._1 == expectedInts)
  }

  // exercise 6.8
  "nonNegativeLessThan" should "return number in correct range" in {
    forAll(minSuccessful(numberOfPropertyBasedTests)) { (seed: Long, targetY: Int) =>
      whenever(targetY > 0) {
        logger.debug(s"targetY [$targetY]")
        val simple = Simple(seed)

        isIntegerBetweenXInclusiveAndYExclusive(nonNegativeLessThanSkewed(targetY)(simple)._1, x = 0, y = targetY);
        isIntegerBetweenXInclusiveAndYExclusive(nonNegativeLessThan(targetY)(simple)._1, x = 0, y = targetY);
        isIntegerBetweenXInclusiveAndYExclusive(nonNegativeLessThanFlatMap(targetY)(simple)._1, x = 0, y = targetY);
      }
    }
  }

  "incorrect die" should "roll a 0" in {
    assert(incorrectRollDie(Simple(5))._1 == 0)
  }

  "die" should "roll a number between 1 and 6" in {
    forAll(minSuccessful(numberOfPropertyBasedTests)) { seed: Long =>
      val roll = rollDie(Simple(seed))._1
      assert(roll >= 1)
      assert(roll <= 6)
    }
  }

  // exercise 6.10
  "state unit" should "return a state" in {
    assert(State.unit[String, Int](2).run("Hello")._1 == 2)
  }

  "state map" should "map state to a new value" in {
    assert(State.unit[String, Int](2).map(_ * 2).run("Hello")._1 == 4)
  }

  "state flatMap" should "map state to a new value" in {
    assert(State.unit[String, Int](2).flatMap(i => State.unit(i * 3)).run("Hello")._1 == 6)
  }

  "state map2" should "map two states together" in {
    assert(State.unit[String, Int](4).map2(State.unit[String, Int](3))(_ * _).run("Hello")._1 == 12)
  }

  "state sequence" should "sequence a list of states" in {
    val l = List(
      State.unit[String, Int](1),
      State.unit[String, Int](2),
      State.unit[String, Int](3)
    )

    assert(State.sequenceViaFoldRight(l).run("Hello")._1 == List(1, 2, 3))
    assert(State.sequenceTextbook(l).run("Hello")._1 == List(1, 2, 3))
    assert(State.sequenceViaFoldLeft(l).run("Hello")._1 == List(1, 2, 3))
    assert(State.sequenceViaFoldLeftTextbook(l).run("Hello")._1 == List(1, 2, 3))
  }

  // pure imperative programs
  "numbers1" should "return a list of numbers" in {
    assert(numbers1.run(Simple(3))._1 == List(-340267510, -140670313, -654558488, -387508411, -260505769, -594271733))
  }

  "numbers2" should "return a list of numbers" in {
    assert(numbers2.run(Simple(3))._1 == List(-340267510, -140670313, -654558488, -387508411, -260505769, -594271733))
  }

  "candy machine" should "dispense two candies" in {
    val inputs = List(Coin, Turn, Coin, Turn)
    val simulation = simulateMachine(inputs)
    val machineStart: Machine = Machine(locked = true, candies = 5, coins = 10)

    val ((coins, candies), machineEnd) = simulation.run(machineStart)

    assert((candies, coins, machineEnd) == (3, 12, Machine(locked = true, candies = 3, coins = 12)))
  }

  it should "not dispense a candy if it's empty" in {
    val inputs = List(Coin, Turn)
    val simulation = simulateMachine(inputs)
    val machineStart: Machine = Machine(locked = true, candies = 0, coins = 10)

    val ((coins, candies), machineEnd) = simulation.run(machineStart)

    assert((candies, coins, machineEnd) == (0, 10, Machine(locked = true, candies = 0, coins = 10)))
  }
}