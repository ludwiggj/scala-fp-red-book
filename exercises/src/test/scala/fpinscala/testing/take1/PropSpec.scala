package fpinscala.testing.take1

import com.typesafe.scalalogging.LazyLogging
import fpinscala.UnitSpec
import fpinscala.state.RNG._
import fpinscala.testing.take1.Gen.{booleanTextbook, choose, choose2, chooseTextbook, chooseTextbook2, listOfN, listOfNTextbook, unitTextbook}
import org.scalacheck.Prop.forAllNoShrink
import org.scalacheck.{Gen => GenOriginal}
import org.scalatestplus.scalacheck.Checkers.check

class PropSpec extends UnitSpec with LazyLogging {
  private def isIntegerBetweenXInclusiveAndYExclusive(x: Int, y: Int, candidate: Int): Boolean = {
    logger.info(s"start [$x] stopExclusive [$y] result [$candidate]")
    assert(candidate >= x)
    assert(candidate < y)
    true
  }

  def stopStartGen(maxStart: Int): GenOriginal[(Int, Int)] = for {
    start <- GenOriginal.choose(0, maxStart)
    stop <- GenOriginal.choose(start + 1, maxStart + 1)
  } yield (start, stop)

  "choose" should "return number in correct range" in {
    check {
      forAllNoShrink(stopStartGen(10), GenOriginal.long) { case ((start, stopExclusive), seed) =>
        isIntegerBetweenXInclusiveAndYExclusive(
          x = start, y = stopExclusive,
          candidate = choose(start, stopExclusive).sample.run(Simple(seed))._1
        )
      }
    }
  }

  "choose2" should "return number in correct range" in {
    check {
      forAllNoShrink(stopStartGen(50), GenOriginal.long) { case ((start, stopExclusive), seed) =>
        isIntegerBetweenXInclusiveAndYExclusive(
          x = start, y = stopExclusive,
          candidate = choose2(start, stopExclusive).sample.run(Simple(seed))._1
        )
      }
    }
  }

  "chooseTextbook" should "return number in correct range" in {
    check {
      forAllNoShrink(stopStartGen(100), GenOriginal.long) { case ((start, stopExclusive), seed) =>
        isIntegerBetweenXInclusiveAndYExclusive(
          x = start, y = stopExclusive,
          candidate = chooseTextbook(start, stopExclusive).sample.run(Simple(seed))._1
        )
      }
    }
  }

  "chooseTextbook2" should "return number in correct range" in {
    check {
      forAllNoShrink(stopStartGen(1000), GenOriginal.long) { case ((start, stopExclusive), seed) =>
        isIntegerBetweenXInclusiveAndYExclusive(
          x = start, y = stopExclusive,
          candidate = chooseTextbook2(start, stopExclusive).sample.run(Simple(seed))._1
        )
      }
    }
  }

  "unit" should "always return the supplied parameter" in {
    assert(Gen.unit(1).sample.run(Simple(1))._1 == 1)
    assert(unitTextbook(1).sample.run(Simple(1))._1 == 1)
  }

  "boolean" should "always return a boolean" in {
    check {
      forAllNoShrink(GenOriginal.long) { seed =>
        val value = Gen.boolean.sample.run(Simple(seed))._1
        val value2 = booleanTextbook.sample.run(Simple(seed))._1
        assert(value == false || value == true)
        assert(value2 == false || value2 == true)
        true
      }
    }
  }

  "listOfN" should "return a list of N elements" in {
    val listLength = 5
    val intGen = choose(1, 10)
    val intListGen = listOfN(listLength, intGen)
    val intListGen2 = listOfNTextbook(listLength, intGen)

    check {
      forAllNoShrink(GenOriginal.long) { seed =>
        val l = intListGen.sample.run(Simple(seed))._1
        val l2 = intListGen2.sample.run(Simple(seed))._1

        println(s"$l $l2")
        assert(l.length == listLength)
        assert(l2.length == listLength)
        true
      }
    }
  }

  it should "return a list of N elements, where N is random" in {
    val gGen: Gen[String] = Gen.unit("G")
    val intGen: Gen[Int] = choose(1, 10)
    val listOfNGs = gGen.listOfN(intGen)
    val listOfNG2 = gGen.listOfNTextbook(intGen)

    check {
      forAllNoShrink(GenOriginal.long) { seed =>

        val l1 = listOfNGs.sample.run(Simple(seed))._1
        val l2 = listOfNG2.sample.run(Simple(seed))._1

        println(s"$l1 $l2")

        assert(l1.nonEmpty && l1.length < 10)
        assert(l2.nonEmpty && l2.length < 10)
        true
      }
    }
  }

  "choosePair" should "return a pair of numbers in range" in {
    val minValue = 0
    val maxValue = 10

    check {
      forAllNoShrink(GenOriginal.long) { seed =>
        val (i1, i2) = Gen.choosePair(minValue, maxValue).sample.run(Simple(seed))._1

        println(s"($i1 $i2)")
        assert(i1 >= minValue && i1 < maxValue)
        assert(i2 >= minValue && i2 < maxValue)
        true
      }
    }
  }

  "toOption" should "return argument wrapped in Some" in {
    val gen = Gen.toOption(Gen.unit(1))
    assert(gen.sample.run(Simple(1))._1.contains(1))
  }

  "fromOption" should "return underlying value" in {
    val gen = Gen.fromOption(Gen.toOption(Gen.unit(1)), default = 5)

    assert(gen.sample.run(Simple(1))._1 == 1)
  }

  it should "return default if generator returns none" in {
    val gen = Gen.fromOption(Gen.unit(Option.empty[Int]), default = 5)

    assert(gen.sample.run(Simple(1))._1 == 5)
  }
}