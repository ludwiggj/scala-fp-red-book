package fpinscala.parallelism

import fpinscala.UnitSpec
import fpinscala.parallelism.ParNonblockingWithErrorHandling.Par._
import fpinscala.parallelism.ParNonblockingWithErrorHandling.Par.toParOps
import scala.util.Right
import java.util.concurrent.{ExecutorService, Executors}

class ParNonblockingSpecWithErrorHandling extends UnitSpec {
  final val unorderedList = List(1, 4, 2, 3, 5)
  final lazy val lazyUnorderedListWithError = List(1, 4, 2 / 0, 3, 5)
  final val divideByZeroMessage = "/ by zero"

  def getService(noOfThreads: Int): ExecutorService = {
    Executors.newFixedThreadPool(noOfThreads)
  }

  private def verifyException[A](either: Either[Exception, A]): Unit = {
    either match {
      case Left(e) =>
        assert(e.getMessage == divideByZeroMessage)
      case _ => fail()
    }
  }

  "lazyUnit" should "return argument" in {
    assert(ParNonblockingWithErrorHandling.Par.run(getService(1))(lazyUnit("no")) == Right("no"))
  }

  it should "handle exception whilst calculating result" in {
    verifyException(ParNonblockingWithErrorHandling.Par.run(getService(1))(lazyUnit(1 / 0)))
  }

  "parLazyUnit" should "return argument" in {
    assert(ParNonblockingWithErrorHandling.Par.run(getService(1))(parLazyUnit("we'll see")) == Right("we'll see"))
  }

  it should "handle exception whilst calculating result" in {
    verifyException(ParNonblockingWithErrorHandling.Par.run(getService(1))(parLazyUnit(1 / 0)))
  }

  "runWithExceptionHandler" should "permit different exception handler" in {
    val service = getService(1)
    val errorMessage = "Exception Time!"

    val result = ParNonblockingWithErrorHandling.Par.runWithExceptionHandler(service)(lazyUnit(1 / 0)) {
      ex =>
        println(errorMessage)
        new Exception(errorMessage, ex)
    }

    result match {
      case Left(e) =>
        assert(e.getMessage == errorMessage)
        assert(e.getCause.getMessage == divideByZeroMessage)
      case _ => fail()
    }
  }

  "parSort" should "sort in parallel" in {
    val sortedList = List(1, 2, 3, 4, 5)

    assert(
      ParNonblockingWithErrorHandling.Par.run(getService(1))(parSort(lazyUnit(unorderedList))) ==
        Right(sortedList)
    )

    assert(
      ParNonblockingWithErrorHandling.Par.run(getService(1))(parSort(unorderedList)) ==
        Right(sortedList)
    )
  }

  it should "handle exception whilst calculating result" in {
    verifyException(
      ParNonblockingWithErrorHandling.Par.run(getService(1))(parSort(lazyUnit(lazyUnorderedListWithError)))
    )

    verifyException(
      ParNonblockingWithErrorHandling.Par.run(getService(1))(parSort(lazyUnorderedListWithError))
    )
  }

  "map" should "map elements" in {
    assert(
      ParNonblockingWithErrorHandling.Par.run(getService(1))(lazyUnit(unorderedList).map(_.map(_ * 2))) ==
        Right(List(2, 8, 4, 6, 10))
    )
  }

  it should "handle exception whilst calculating result" in {
    verifyException(
      ParNonblockingWithErrorHandling.Par.run(getService(1))(lazyUnit(lazyUnorderedListWithError).map(_.map(_ * 2)))
    )
  }

  "sequence" should "sequence computations" in {
    val service = getService(1)
    val input = List(lazyUnit(2), lazyUnit(4), lazyUnit(6), lazyUnit(8))
    val expected = Right(List(2, 4, 6, 8))

    assert(ParNonblockingWithErrorHandling.Par.run(service)(sequence(input)) == expected)
    assert(ParNonblockingWithErrorHandling.Par.run(service)(sequenceRight(input)) == expected)
    assert(ParNonblockingWithErrorHandling.Par.run(service)(sequenceBalanced(input.toIndexedSeq)) == expected)
  }

  it should "handle exception whilst calculating result" in {
    val service = getService(1)
    val input = List(lazyUnit(2), lazyUnit(4 / 0), lazyUnit(6), lazyUnit(8))

    verifyException(ParNonblockingWithErrorHandling.Par.run(service)(sequence(input)))
    verifyException(ParNonblockingWithErrorHandling.Par.run(service)(sequenceRight(input)))
    verifyException(ParNonblockingWithErrorHandling.Par.run(service)(sequenceBalanced(input.toIndexedSeq)))
  }

  "parMap" should "map elements in parallel" in {
    assert(
      ParNonblockingWithErrorHandling.Par.run(getService(1))(parMap(unorderedList)(_ * 2)) ==
        Right(List(2, 8, 4, 6, 10))
    )

    assert(
      ParNonblockingWithErrorHandling.Par.run(getService(1))(parMapFixed(unorderedList)(_ * 2)) ==
        Right(List(2, 8, 4, 6, 10))
    )
  }

  // TODO - needs fixing
  it should "handle exception in list whilst calculating result BUT IT DOESN'T" in {
    verifyException(
      ParNonblockingWithErrorHandling.Par.run(getService(1))(parMap(lazyUnorderedListWithError)(_ * 2))
    )
  }

  it should "handle exception in list whilst calculating result" in {
    verifyException(
      ParNonblockingWithErrorHandling.Par.run(getService(1))(parMapFixed(lazyUnorderedListWithError)(_ * 2))
    )
  }

  it should "handle exception in mapping function whilst calculating result" in {
    verifyException(
      ParNonblockingWithErrorHandling.Par.run(getService(1))(parMap(unorderedList)(_ / 0))
    )

    verifyException(
      ParNonblockingWithErrorHandling.Par.run(getService(1))(parMapFixed(unorderedList)(_ / 0))
    )
  }

  "map2" should "run using actors" in {
    assert(
      ParNonblockingWithErrorHandling.Par.run(getService(1))(parMap(List.range(1, 10))(x => math.pow(x, 2)))
        == Right(List(1, 4, 9, 16, 25, 36, 49, 64, 81))
    )
  }

  "parFilter" should "filter elements in parallel" in {
    assert(
      ParNonblockingWithErrorHandling.Par.run(getService(1))(parFilter(unorderedList)(_ % 2 == 0))
        == Right(List(4, 2))
    )

    assert(
      ParNonblockingWithErrorHandling.Par.run(getService(1))(parFilterFixed(unorderedList)(_ % 2 == 0))
        == Right(List(4, 2))
    )
  }

  // TODO - needs fixing
  it should "handle exception in list whilst calculating result BUT IT DOESN'T" in {
    verifyException(
      ParNonblockingWithErrorHandling.Par.run(getService(1))(
        parFilter(lazyUnorderedListWithError)(_ % 2 == 0)
      )
    )
  }

  it should "handle exception in list whilst calculating result" in {
    verifyException(
      ParNonblockingWithErrorHandling.Par.run(getService(1))(
        parFilterFixed(lazyUnorderedListWithError)(_ % 2 == 0)
      )
    )
  }

  it should "handle exception in predicate whilst calculating result" in {
    verifyException(
      ParNonblockingWithErrorHandling.Par.run(getService(1))(parFilter(unorderedList)(_ % 0 == 1))
    )

    verifyException(
      ParNonblockingWithErrorHandling.Par.run(getService(1))(parFilterFixed(unorderedList)(_ % 0 == 1))
    )
  }

  "choice" should "return first option if argument is true" in {
    val service = getService(1)

    assert(
      ParNonblockingWithErrorHandling.Par.run(service)(choice(lazyUnit(true))(lazyUnit("yes"), lazyUnit("no")))
        == Right("yes")
    )

    assert(
      ParNonblockingWithErrorHandling.Par.run(service)(choiceViaChoiceN(lazyUnit(true))(
        lazyUnit("yes"), lazyUnit("no")
      )) == Right("yes")
    )

    assert(
      ParNonblockingWithErrorHandling.Par.run(service)(choiceViaChooser(lazyUnit(true))(
        lazyUnit("yes"), lazyUnit("no")
      )) == Right("yes")
    )
  }

  it should "return second option if argument is false" in {
    val service = getService(1)

    assert(
      ParNonblockingWithErrorHandling.Par.run(service)(choice(lazyUnit(false))(lazyUnit("yes"), lazyUnit("no")))
        == Right("no")
    )

    assert(
      ParNonblockingWithErrorHandling.Par.run(service)(choiceViaChoiceN(lazyUnit(false))(
        lazyUnit("yes"), lazyUnit("no")
      )) == Right("no")
    )

    assert(
      ParNonblockingWithErrorHandling.Par.run(service)(choiceViaChooser(lazyUnit(false))(
        lazyUnit("yes"), lazyUnit("no")
      )) == Right("no")
    )
  }

  it should "handle exception whilst calculating result" in {
    val service = getService(1)

    verifyException(
      ParNonblockingWithErrorHandling.Par.run(service)(choice(lazyUnit({
        1 / 0
        false
      }))(
        lazyUnit("yes"), lazyUnit("no")
      ))
    )

    verifyException(
      ParNonblockingWithErrorHandling.Par.run(service)(choice(lazyUnit({
        false
      }))(
        lazyUnit("yes"), lazyUnit({
          1 / 0
          "no"
        })
      ))
    )

    verifyException(
      ParNonblockingWithErrorHandling.Par.run(service)(choiceViaChoiceN(lazyUnit({
        1 / 0
        false
      }))(
        lazyUnit("yes"), lazyUnit("no")
      ))
    )

    verifyException(
      ParNonblockingWithErrorHandling.Par.run(service)(choiceViaChoiceN(lazyUnit({
        false
      }))(
        lazyUnit("yes"), lazyUnit({
          1 / 0
          "no"
        })
      ))
    )

    verifyException(
      ParNonblockingWithErrorHandling.Par.run(service)(choiceViaChooser(lazyUnit({
        1 / 0
        false
      }))(
        lazyUnit("yes"), lazyUnit("no")
      ))
    )

    verifyException(
      ParNonblockingWithErrorHandling.Par.run(service)(choiceViaChooser(lazyUnit({
        false
      }))(
        lazyUnit("yes"), lazyUnit({
          1 / 0
          "no"
        })
      ))
    )
  }

  it should "ignore exception if it is not evaluated" in {
    val service = getService(1)

    assert(
      ParNonblockingWithErrorHandling.Par.run(service)(choice(lazyUnit(false))(
        lazyUnit({
          1 / 0
          "yes"
        }), lazyUnit("no")
      )) == Right("no")
    )

    assert(
      ParNonblockingWithErrorHandling.Par.run(service)(choiceViaChoiceN(lazyUnit(false))(
        lazyUnit({
          1 / 0
          "yes"
        }), lazyUnit("no")
      )) == Right("no")
    )

    assert(
      ParNonblockingWithErrorHandling.Par.run(service)(choiceViaChooser(lazyUnit(false))(
        lazyUnit({
          1 / 0
          "yes"
        }), lazyUnit("no")
      )) == Right("no")
    )
  }

  "choiceN" should "return selected option" in {
    val service = getService(1)

    assert(
      ParNonblockingWithErrorHandling.Par.run(service)(choiceN(lazyUnit(1))(
        List(lazyUnit(3), lazyUnit(2), lazyUnit(1))
      )) == Right(2)
    )

    assert(
      ParNonblockingWithErrorHandling.Par.run(service)(choiceNViaChooser(lazyUnit(1))(
        List(lazyUnit(3), lazyUnit(2), lazyUnit(1))
      )) == Right(2)
    )
  }

  it should "handle exception whilst calculating result" in {
    verifyException(
      ParNonblockingWithErrorHandling.Par.run(getService(1))(choiceN(lazyUnit(1))(
        List(lazyUnit(3), lazyUnit(2 / 0), lazyUnit(1))
      ))
    )

    verifyException(
      ParNonblockingWithErrorHandling.Par.run(getService(1))(choiceNViaChooser(lazyUnit(1))(
        List(lazyUnit(3), lazyUnit(2 / 0), lazyUnit(1))
      ))
    )
  }

  it should "ignore exception if it is not evaluated" in {
    val service = getService(1)

    assert(
      ParNonblockingWithErrorHandling.Par.run(service)(choiceN(lazyUnit(1))(
        List(lazyUnit(3 / 0), lazyUnit(2), lazyUnit(1))
      )) == Right(2)
    )

    assert(
      ParNonblockingWithErrorHandling.Par.run(service)(choiceNViaChooser(lazyUnit(1))(
        List(lazyUnit(3 / 0), lazyUnit(2), lazyUnit(1))
      )) == Right(2)
    )
  }

  "choiceMap" should "return selected option" in {
    assert(ParNonblockingWithErrorHandling.Par.run(getService(1))(choiceMap(lazyUnit("brown"))(Map(
      "how" -> lazyUnit(3),
      "now" -> lazyUnit(3),
      "brown" -> lazyUnit(5),
      "cow" -> lazyUnit(3)
    ))) == Right(5))
  }

  it should "handle exception whilst calculating result" in {
    verifyException(ParNonblockingWithErrorHandling.Par.run(getService(1))(choiceMap(lazyUnit("now"))(Map(
      "how" -> lazyUnit(3),
      "now" -> lazyUnit(3 / 0),
      "brown" -> lazyUnit(5),
      "cow" -> lazyUnit(3)
    ))))
  }

  it should "ignore exception if it is not evaluated" in {
    assert(ParNonblockingWithErrorHandling.Par.run(getService(1))(choiceMap(lazyUnit("brown"))(Map(
      "how" -> lazyUnit(3),
      "now" -> lazyUnit(3 / 0),
      "brown" -> lazyUnit(5),
      "cow" -> lazyUnit(3)
    ))) == Right(5))
  }

  def choose[A](input: String): ParNonblockingWithErrorHandling.Par[Int] = input match {
    case "red" => lazyUnit(1)
    case "orange" => lazyUnit(2)
    case "yellow" => lazyUnit(3)
    case "green" => lazyUnit(4)
    case "blue" => lazyUnit(5)
    case "indigo" => lazyUnit(6)
    case "violet" => lazyUnit(7)
  }

  def chooseWithBoobyTrap[A](input: String): ParNonblockingWithErrorHandling.Par[Int] = input match {
    case "red" => lazyUnit(1)
    case "orange" => lazyUnit(2)
    case "yellow" => lazyUnit(3)
    case "green" => lazyUnit(4 / 0)
    case "blue" => lazyUnit(5)
    case "indigo" => lazyUnit(6)
    case "violet" => lazyUnit(7)
  }

  "chooser" should "return selected option" in {
    assert(
      ParNonblockingWithErrorHandling.Par.run(getService(1))(chooser(lazyUnit("green"))(choose))
        == Right(4)
    )
  }

  it should "handle exception whilst calculating result" in {
    verifyException(
      ParNonblockingWithErrorHandling.Par.run(getService(1))(
        chooser(lazyUnit("green"))(chooseWithBoobyTrap)
      )
    )
  }

  it should "ignore exception if it is not evaluated" in {
    assert(ParNonblockingWithErrorHandling.Par.run(getService(1))(
      chooser(lazyUnit("orange"))(choose)
    ) == Right(2))
  }

  "join" should "join computations" in {
    val service = getService(1)

    assert(ParNonblockingWithErrorHandling.Par.run(service)(join(lazyUnit(lazyUnit(1)))) == Right(1))
    assert(ParNonblockingWithErrorHandling.Par.run(service)(joinViaFlatMap(lazyUnit(lazyUnit(1)))) == Right(1))
  }

  // TODO - fix needed
  ignore should "handle exception whilst calculating result BUT IT DOESN'T" in {
    verifyException(ParNonblockingWithErrorHandling.Par.run(getService(1))(join(lazyUnit(lazyUnit(1 / 0)))))
  }

  it should "handle exception whilst calculating result" in {
    verifyException(ParNonblockingWithErrorHandling.Par.run(getService(1))(
      joinViaFlatMap(lazyUnit(lazyUnit(1 / 0)))
    ))
  }
}