package fpinscala.parallelism

import fpinscala.UnitSpec
import fpinscala.parallelism.ParNonblocking.Par._
import fpinscala.errorhandling.{Left, Right}
import fpinscala.errorhandling.Either.Try

import java.util.concurrent.{ExecutorService, Executors}

class ParNonblockingSpec extends UnitSpec {
  final val unorderedList = List(1, 4, 2, 3, 5)

  def getService(noOfThreads: Int): ExecutorService = {
    Executors.newFixedThreadPool(noOfThreads)
  }

  "unit" should "return argument" in {
    val service = getService(1)

    assert(ParNonblocking.Par.run(service)(unit("no")) == "no")
  }

  "run" should "return computation" in {
    val service = getService(1)

    assert(ParNonblocking.Par.run(service)(unit("maybe")) == "maybe")
  }

  "lazyUnit" should "run using only 1 thread" in {
    // NOTE - Smaller number of threads needed
    val service = getService(1)

    assert(ParNonblocking.Par.run(service)(lazyUnit("we'll see")) == "we'll see")
  }

  "parSort" should "sort in parallel" in {
    val service = getService(1)

    val expected = List(1, 2, 3, 4, 5)

    assert(ParNonblocking.Par.run(service)(parSort(unit(unorderedList))) == expected)
  }

  "map" should "map elements" in {
    val service = getService(1)

    assert(ParNonblocking.Par.run(service)(unit(unorderedList).map(_.map(_ * 2))) == List(2, 8, 4, 6, 10))
  }

  "sequence" should "sequence computations using 1 thread" in {
    val service = getService(1)
    val input = List(unit(2), unit(4), unit(6), unit(8))
    val expected = List(2, 4, 6, 8)

    assert(ParNonblocking.Par.run(service)(sequence(input)) == expected)
    assert(ParNonblocking.Par.run(service)(sequenceRight(input)) == expected)
    assert(ParNonblocking.Par.run(service)(sequenceBalanced(input.toIndexedSeq)) == expected)
  }

  "parMap" should "map elements in parallel" in {
    // NOTE - Smaller number of threads needed
    val service = getService(1)

    assert(ParNonblocking.Par.run(service)(parMap(unorderedList)(_ * 2)) == List(2, 8, 4, 6, 10))
  }

  "map2" should "run using actors" in {
    // NOTE - Smaller number of threads needed
    val service = getService(1)

    val result: List[Double] = ParNonblocking.Par.run(service)(
      parMap(List.range(1, 10))(x => math.pow(x, 2))
    )

    assert(result == List(1, 4, 9, 16, 25, 36, 49, 64, 81))
  }

  "parFilter" should "filter elements in parallel" in {
    // NOTE - Smaller number of threads needed
    val service = getService(1)
    val expected = List(4, 2)

    assert(ParNonblocking.Par.run(service)(parFilter(unorderedList)(_ % 2 == 0)) == expected)
  }

  "choice" should "return first option if argument is true" in {
    // NOTE - Smaller number of threads needed
    val service = getService(1)

    assert(ParNonblocking.Par.run(service)(choice(unit(true))(unit("yes"), unit("no"))) == "yes")
    assert(ParNonblocking.Par.run(service)(choiceViaChoiceN(unit(true))(unit("yes"), unit("no"))) == "yes")
    assert(ParNonblocking.Par.run(service)(choiceViaChooser(unit(true))(unit("yes"), unit("no"))) == "yes")
  }

  it should "return second option if argument is false" in {
    // NOTE - Smaller number of threads needed
    val service = getService(1)

    assert(ParNonblocking.Par.run(service)(choice(unit(false))(unit("yes"), unit("no"))) == "no")
    assert(ParNonblocking.Par.run(service)(choiceViaChoiceN(unit(false))(unit("yes"), unit("no"))) == "no")
    assert(ParNonblocking.Par.run(service)(choiceViaChooser(unit(false))(unit("yes"), unit("no"))) == "no")
  }

  "choiceN" should "return selected option" in {
    // NOTE - Smaller number of threads needed
    val service = getService(1)

    assert(ParNonblocking.Par.run(service)(choiceN(unit(1))(List(unit(3), unit(2), unit(1)))) == 2)
    assert(ParNonblocking.Par.run(service)(choiceNViaChooser(unit(1))(List(unit(3), unit(2), unit(1)))) == 2)
  }

  "choiceMap" should "return selected option" in {
    // NOTE - Smaller number of threads needed
    val service = getService(1)

    assert(ParNonblocking.Par.run(service)(choiceMap(unit("brown"))(Map(
      "how" -> unit(3),
      "now" -> unit(3),
      "brown" -> unit(5),
      "cow" -> unit(3)
    ))) == 5)
  }

  "chooser" should "return selected option" in {
    // NOTE - Smaller number of threads needed
    val service = getService(1)

    def choose[A](input: String):ParNonblocking.Par[Int] = input match {
      case "red" => unit(1)
      case "orange" => unit(2)
      case "yellow" => unit(3)
      case "green" => unit(4)
      case "blue" => unit(5)
      case "indigo" => unit(6)
      case "violet" => unit(7)
    }

    assert(ParNonblocking.Par.run(service)(chooser(unit("green"))(choose)) == 4)
    assert(ParNonblocking.Par.run(service)(flatMap(unit("green"))(choose)) == 4)
  }

  "join" should "join computations" in {
    // NOTE - Smaller number of threads needed
    val service = getService(1)

    assert(ParNonblocking.Par.run(service)(join(unit(unit(1)))) == 1)
    assert(ParNonblocking.Par.run(service)(joinViaFlatMap(unit(unit(1)))) == 1)
  }

  "run2" should "handle incorrect result" in {
    // NOTE - Smaller number of threads needed
    val service = getService(1)

    val result = ParNonblocking.Run2.run(service)(lazyUnit(Try(1 / 0)))

    result match {
      case Left(e) => assert(e.getMessage == "/ by zero")
      case _ => fail()
    }
  }

  it should "handle correct result" in {
    // NOTE - Smaller number of threads needed
    val service = getService(1)

    val result = ParNonblocking.Run2.run(service)(lazyUnit(Try(1)))

    result match {
      case Right(v) => assert(v == 1)
      case _ => fail()
    }
  }
}