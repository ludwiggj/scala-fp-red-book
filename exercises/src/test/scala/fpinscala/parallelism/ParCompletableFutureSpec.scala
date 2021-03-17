package fpinscala.parallelism

import fpinscala.UnitSpec
import fpinscala.parallelism.ParCompletableFuture.{chooser, _}

import java.util.concurrent.{Callable, ExecutorService, Executors}

class ParCompletableFutureSpec extends UnitSpec {
  final val unorderedList = List(1, 4, 2, 3, 5)

  final val exampleParagraphs = List(
    """ Shoppers who may not want to receive adverts for Mother’s Day cards, flowers and presents in the run-up to next
      | Sunday are being sent emails asking if they want to opt out of the messages.
      |""".stripMargin,
    """ Two years ago the Tory MP Matt Warman called for advertisers to adopt op-out policies, telling parliament of his
      | own “dread” at receiving marketing messages after the death of his parents.
      |""".stripMargin,
    """ At the time, the online florist Bloom & Wild was offering its customers the chance to opt-out of emails, and
      | Warman suggested that others could be asked to do similar as part of a voluntary code.
      |""".stripMargin,
    """ Many big companies are offering this for the first time this year, with some citing the pandemic as a reason for
      | introducing the opt-out.
      |""".stripMargin,
    """ But while some customers have welcomed the choice, others have suggested that the messages could be more
      | upsetting than the emails they are designed to stop.
      |""".stripMargin,
    """ The online retailer Very.co.uk emailed customers in February asking them to click on a link if they did not want
      | to receive Mother’s Day reminders.
      |""".stripMargin
  )

  def getService(noOfThreads: Int): ExecutorService = {
    Executors.newFixedThreadPool(noOfThreads)
  }

  "service" should "execute callable" in {
    val callable = new Callable[String]() {
      def call(): String = {
        "yes"
      }
    }
    val service = getService(1)

    assert(service.submit(callable).get() == "yes")
  }

  "unit" should "return argument on get" in {
    val service = getService(1)

    assert(ParCompletableFuture.unit("no")(service).get() == "no")
  }

  "run" should "return computation" in {
    val service = getService(1)

    assert(ParCompletableFuture.run(service)(unit("maybe")).get() == "maybe")
  }

  "lazyUnit" should "run if 2 threads are available" in {
    val service = getService(2)

    assert(ParCompletableFuture.run(service)(lazyUnit("we'll see")).get() == "we'll see")
  }

  "parSort" should "sort in parallel" in {
    val service = getService(1)

    val expected = List(1, 2, 3, 4, 5)

    assert(ParCompletableFuture.run(service)(parSort(unit(unorderedList))).get() == expected)
    assert(ParCompletableFuture.run(service)(parSort2(unit(unorderedList))).get() == expected)
  }

  it should "sort in parallel with more threads" in {
    val service = getService(3)

    val expected = List(1, 2, 3, 4, 5)
    assert(ParCompletableFuture.run(service)(parSort3(unit(unorderedList))).get() == expected)
  }

  "map" should "map elements" in {
    val service = getService(1)

    assert(ParCompletableFuture.run(service)(unit(unorderedList).map(_.map(_ * 2))).get() == List(2, 8, 4, 6, 10))
  }

  "sequence" should "sequence computations using 1 thread" in {
    val service = getService(1)
    val input = List(unit(2), unit(4), unit(6), unit(8))
    val expected = List(2, 4, 6, 8)

    assert(ParCompletableFuture.run(service)(sequence(input)).get() == expected)

    // NOTE - Result comes out in reverse order for this method
    assert(ParCompletableFuture.run(service)(sequence2(input)).get().reverse == expected)

    assert(ParCompletableFuture.run(service)(sequence_simpleTextbook(input)).get() == expected)
  }

  it should "sequence computations using 5 threads if fork is called" in {
    val service = getService(5)
    val input = List(unit(2), unit(4), unit(6), unit(8))
    val expected = List(2, 4, 6, 8)

    assert(ParCompletableFuture.run(service)(sequenceRightTextbook(input)).get() == expected)

    // NOTE - Result comes out in reverse order for this method
    assert(ParCompletableFuture.run(service)(sequenceBalancedTextbook(input.toIndexedSeq)).get() == List(2, 4, 6, 8))

    assert(ParCompletableFuture.run(service)(sequenceTextbook(input)).get() == List(2, 4, 6, 8))
  }

  "parMap" should "map elements in parallel" in {
    val service = getService(10)

    assert(ParCompletableFuture.run(service)(parMap(unorderedList)(_ * 2)).get() == List(2, 8, 4, 6, 10))
  }

  "parFilter" should "filter elements in parallel" in {
    val service = getService(2)
    val expected = List(4, 2)

    assert(ParCompletableFuture.run(service)(parFilter(unorderedList)(_ % 2 == 0)).get() == expected)
    assert(ParCompletableFuture.run(service)(parFilter2(unorderedList)(_ % 2 == 0)).get() == expected)
  }

  "sum" should "return element total" in {
    val service = getService(1)
    val input = unorderedList.toIndexedSeq

    assert(ParCompletableFuture.run(service)(sum(input)).get() == 15)
    assert(ParCompletableFuture.run(service)(sumViaFold1(input)).get() == 15)
    assert(ParCompletableFuture.run(service)(sumViaFold2(input)).get() == 15)
  }

  "max" should "return largest element" in {
    val service = getService(1)
    val input = IndexedSeq(2, 5, -12, 3, 0)

    assert(ParCompletableFuture.run(service)(maxValueViaFold1(input)).get() == 5)
    assert(ParCompletableFuture.run(service)(maxValueViaFold2(input)).get() == 5)
  }

  "wordCount" should "return number of words in paragraphs" in {
    val service = getService(1)
    val expectedCount = 183

    assert(wc1(exampleParagraphs) == expectedCount)
    assert(ParCompletableFuture.run(service)(wc2(exampleParagraphs)).get() == expectedCount)
    assert(ParCompletableFuture.run(service)(wc3(exampleParagraphs)(_.split(" ").length)(_ + _)).get() == expectedCount)
    assert(ParCompletableFuture.run(service)(wordCount(exampleParagraphs)).get() == expectedCount)
  }

  "map3" should "map 3 Pars together" in {
    val service = getService(1)
    assert(
      ParCompletableFuture.run(service)(map3(unit("Three "), unit("Blind "), unit("Mice"))(_ + _ + _)).get()
        == "Three Blind Mice"
    )
  }

  "map4" should "map 4 Pars together" in {
    val service = getService(1)
    assert(ParCompletableFuture.run(service)(map4(unit(4), unit(3), unit(2), unit(1))(_ * _ * _ * _)).get() == 24)
  }

  "map5" should "map 5 Pars together" in {
    val service = getService(1)
    assert(ParCompletableFuture.run(service)(map5(unit(5), unit(4), unit(3), unit(2), unit(1))(_ + _ + _ + _ + _)).get() == 15)
  }

  "equal" should "return true if Pars return equal values" in {
    val service = getService(1)
    assert(ParCompletableFuture.equal(service)(unit(2 + 2), unit(3 + 1)))
  }

  it should "return false if Pars return unequal values" in {
    val service = getService(1)
    assert(!ParCompletableFuture.equal(service)(unit(2 + 1), unit(3 + 1)))
  }

  "choice" should "return first option if argument is true" in {
    val service = getService(2)
    assert(ParCompletableFuture.run(service)(choice(unit(true))(unit("yes"), unit("no"))).get() == "yes")
    assert(ParCompletableFuture.run(service)(choiceViaChoiceN(unit(true))(unit("yes"), unit("no"))).get() == "yes")
    assert(ParCompletableFuture.run(service)(choiceViaChooser(unit(true))(unit("yes"), unit("no"))).get() == "yes")
  }

  it should "return second option if argument is false" in {
    val service = getService(2)
    assert(ParCompletableFuture.run(service)(choice(unit(false))(unit("yes"), unit("no"))).get() == "no")
    assert(ParCompletableFuture.run(service)(choiceViaChoiceN(unit(false))(unit("yes"), unit("no"))).get() == "no")
    assert(ParCompletableFuture.run(service)(choiceViaChooser(unit(false))(unit("yes"), unit("no"))).get() == "no")
  }

  "choiceN" should "return selected option" in {
    val service = getService(2)
    assert(ParCompletableFuture.run(service)(choiceN(unit(1))(List(unit(3), unit(2), unit(1)))).get() == 2)
    assert(ParCompletableFuture.run(service)(choiceNViaChooser(unit(1))(List(unit(3), unit(2), unit(1)))).get() == 2)
  }

  "choiceMap" should "return selected option" in {
    val service = getService(2)
    assert(ParCompletableFuture.run(service)(choiceMap(unit("brown"))(Map(
      "how" -> unit(3),
      "now" -> unit(3),
      "brown" -> unit(5),
      "cow" -> unit(3)
    ))).get() == 5)
  }

  "chooser" should "return selected option" in {
    val service = getService(2)

    def choose[A](input: String): Par[Int] = input match {
      case "red" => unit(1)
      case "orange" => unit(2)
      case "yellow" => unit(3)
      case "green" => unit(4)
      case "blue" => unit(5)
      case "indigo" => unit(6)
      case "violet" => unit(7)
    }

    assert(ParCompletableFuture.run(service)(chooser(unit("green"))(choose)).get() == 4)
    assert(ParCompletableFuture.run(service)(flatMap(unit("green"))(choose)).get() == 4)
  }

  "join" should "join computations" in {
    val service = getService(2)

    assert(ParCompletableFuture.run(service)(join(unit(unit(1)))).get() == 1)
    assert(ParCompletableFuture.run(service)(joinViaFlatmap(unit(unit(1)))).get() == 1)
    assert(ParCompletableFuture.run(service)(joinAlternative(unit(unit(1)))).get() == 1)
  }
}