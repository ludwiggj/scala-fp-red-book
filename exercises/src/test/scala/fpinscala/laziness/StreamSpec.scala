package fpinscala.laziness

import fpinscala.UnitSpec

class StreamSpec extends UnitSpec {
  private val emptyStream = Stream[Int]()
  private val stream1to3 = Stream(1, 2, 3)
  private val stream1to5 = Stream(1, 2, 3, 4, 5)
  private val emptyList = List[Int]()
  private val list1to3 = List(1, 2, 3)
  private val list1to5 = List(1, 2, 3, 4, 5)

  def applyTest(stream: Stream[Int], expectedList: List[Int])(toList: Stream[Int] => List[Int]): Unit = {
    assert(toList(stream) === expectedList)
  }

  def applyTest(stream: Stream[Int],
                p: Int,
                expectedList: List[Int])(f: Stream[Int] => Int => Stream[Int]): Unit = {
    assert(f(stream)(p).toList === expectedList)
  }

  def applyTest(stream: Stream[Int],
                p: Int => Boolean,
                expectedList: List[Int])(f: Stream[Int] => (Int => Boolean) => Stream[Int]): Unit = {
    assert(f(stream)(p).toList === expectedList)
  }

  def applyTest(stream: Stream[Int],
                p: Int => Boolean,
                expected: Boolean)(f: Stream[Int] => (Int => Boolean) => Boolean): Unit = {
    assert(f(stream)(p) === expected)
  }

  def applyTest(stream: Stream[Int],
                expected: Option[Int])(f: Stream[Int] => Option[Int]): Unit = {
    assert(f(stream) === expected)
  }

  def applyLazyTest(stream: Stream[Int],
             p: => Int,
             expectedList: List[Int])(f: Stream[Int] => (=> Int) => Stream[Int]): Unit = {
    assert(f(stream)(p).toList === expectedList)
  }

  // Exercise 5.1

  // Call multiple implementations of same method in same test
  def runToListTests(toListTest: (Stream[Int] => List[Int]) => Unit): Unit = {
    toListTest({ s: Stream[Int] => s.toListRecursiveTextbook })
    toListTest(_.toListRecursive)
    toListTest(_.toList)
    toListTest(_.toListFast)
    toListTest(_.toListViaFoldRight)
  }

  "toList" should "return empty list if stream is empty" in {
    runToListTests(applyTest(emptyStream, emptyList))
  }

  it should "return list containing same elements in the same order" in {
    runToListTests(applyTest(stream1to3, list1to3))
  }

  // Exercise 5.2

  // Call multiple implementations of same method in same test
  def runTakeTests(takeTest: (Stream[Int] => Int => Stream[Int]) => Unit): Unit = {
    takeTest({ s: Stream[Int] => s.take })
    takeTest(_.takeFast)
    takeTest(_.takeTextbook)
  }

  "take" should "return first n elements if take n" in {
    runTakeTests(applyTest(stream1to5, 3, list1to3))
  }

  it should "return all elements if take all" in {
    runTakeTests(applyTest(stream1to5, 5, list1to5))
  }

  it should "return no elements if take a negative number" in {
    runTakeTests(applyTest(stream1to5, -1, emptyList))
  }

  it should "return all elements if take more elements than are present" in {
    runTakeTests(applyTest(stream1to5, 6, list1to5))
  }

  // Call multiple implementations of same method in same test
  def runDropTests(dropTest: (Stream[Int] => Int => Stream[Int]) => Unit): Unit = {
    dropTest({ s: Stream[Int] => s.drop })
    dropTest(_.dropTextbook)
  }

  "drop" should "return all elements after n if drop n" in {
    runDropTests(applyTest(stream1to5, 3, List(4, 5)))
  }

  it should "return no elements if drop all of them" in {
    runDropTests(applyTest(stream1to5, 5, emptyList))
  }

  it should "return all elements if drop a negative number" in {
    runDropTests(applyTest(stream1to5, -1, list1to5))
  }

  it should "return no elements if drop more elements than are present" in {
    runDropTests(applyTest(stream1to5, 6, emptyList))
  }

  // Exercises 5.3 and 5.5

  // Call multiple implementations of same method in same test
  def runTakeWhileTests(takeWhileTest: (Stream[Int] => (Int => Boolean) => Stream[Int]) => Unit): Unit = {
    takeWhileTest({ s: Stream[Int] => s.takeWhile })
    takeWhileTest(_.takeWhileTextbook)
    takeWhileTest(_.takeWhileViaFoldRight)
  }

  "takeWhile" should "return elements until first element that does not satisfy criteria" in {
    runTakeWhileTests(applyTest(stream1to5, _ < 3, List(1, 2)))
  }

  it should "return all elements if they all satisfy criteria" in {
    runTakeWhileTests(applyTest(stream1to5, _ < 6, list1to5))
  }

  it should "return an empty list if first element does not satisfy criteria" in {
    runTakeWhileTests(applyTest(stream1to5, _ > 2, emptyList))
  }

  it should "return even numbers" in {
    runTakeWhileTests(applyTest(Stream(2, 4, 8, 0, 5), _ % 2 == 0, List(2, 4, 8, 0)))
  }

  // Call multiple implementations of same method in same test
  def runExistsTests(existsTest: (Stream[Int] => (Int => Boolean) => Boolean) => Unit): Unit = {
    existsTest({ s: Stream[Int] => s.exists })
    existsTest(_.existsRecursive)
  }

  "exists" should "return true if matching element is present" in {
    runExistsTests(applyTest(stream1to5, _ == 5, true))
  }

  it should "return true if more than one matching element is present" in {
    runExistsTests(applyTest(stream1to5, _ % 2 == 0, true))
  }

  it should "return false if no matching elements are present" in {
    runExistsTests(applyTest(stream1to5, _ % 6 == 0, false))
  }

  // Exercise 5.4

  // Call multiple implementations of same method in same test
  def runForAllTests(forAllTest: (Stream[Int] => (Int => Boolean) => Boolean) => Unit): Unit = {
    forAllTest({ s: Stream[Int] => s.forAll })
    forAllTest(_.forAllRecursive)
  }

  "forAll" should "return true if all elements satisfy criteria" in {
    runForAllTests(applyTest(stream1to5, _ < 6, true))
  }

  it should "return false if some elements do not satisfy criteria" in {
    runForAllTests(applyTest(stream1to5, _ < 5, false))
  }

  it should "return false if all elements do not satisfy criteria" in {
    runForAllTests(applyTest(stream1to5, _ < 0, false))
  }

  // Exercise 5.6

  // Call multiple implementations of same method in same test
  def runHeadOptionTests(headOptionTest: (Stream[Int] => Option[Int]) => Unit): Unit = {
    headOptionTest({ s: Stream[Int] => s.headOption })
    headOptionTest(_.headOptionViaFoldRight)
  }

  "headOption" should "return None if stream is empty" in {
    runHeadOptionTests(applyTest(emptyStream, None))
  }

  it should "return first element if stream is not empty" in {
    runHeadOptionTests(applyTest(stream1to3, Some(1)))
  }

  // Exercise 5.7
  "map" should "apply function to every element" in {
    val list1to5Floats = List(1.0, 2.0, 3.0, 4.0, 5.0)

    assert(stream1to5.mapRecursive(_.toFloat).toList === list1to5Floats)
    assert(stream1to5.map(_.toFloat).toList === list1to5Floats)
  }

  "filter" should "return elements that satisfy criteria" in {
    assert(stream1to5.filter(_ % 2 == 1).toList == List(1, 3, 5))
  }

  it should "return all elements if they all satisfy criteria" in {
    assert(stream1to5.filter(_ > 0).toList == list1to5)
  }

  it should "return no elements if none of them satisfy criteria" in {
    assert(stream1to5.filter(_ < 0).toList == emptyList)
  }

  // Call multiple implementations of same method in same test
  def runAppendElemTests(appendElemTest: (Stream[Int] => (=> Int) => Stream[Int]) => Unit): Unit = {
    appendElemTest({ s: Stream[Int] => s.appendElemTextbook })
    appendElemTest(_.appendElem)
  }

  "appendElem" should "return existing stream with element appended" in {
    runAppendElemTests(applyLazyTest(stream1to5, 6, Range.inclusive(1, 6).toList))
  }

  it should "return single element stream when append to empty stream" in {
    runAppendElemTests(applyLazyTest(emptyStream, 1, List(1)))
  }

  "append" should "add a stream to the end of another" in {
    assert(stream1to5.append(Stream(6, 7, 8, 9, 10)).toList == Range.inclusive(1, 10).toList)
  }

  "flatMap" should "flatmap the stream :)" in {
    def toStream(x: Int): Stream[Int] = {
      Stream(x, 2 * x)
    }
    val expected = List(1, 2, 2, 4, 3, 6, 4, 8, 5, 10)

    assert(stream1to5.flatMap(toStream).toList == expected)
    assert(stream1to5.flatMapTextbook(toStream).toList == expected)
  }
}