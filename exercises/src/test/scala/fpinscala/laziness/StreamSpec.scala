package fpinscala.laziness

import fpinscala.UnitSpec
import fpinscala.laziness.Stream._

class StreamSpec extends UnitSpec {
  private val emptyStream = Stream[Int]()
  private val stream1to3 = Stream(1, 2, 3)
  private val stream1to5 = Stream(1, 2, 3, 4, 5)
  private val emptyList = List[Int]()
  private val list1to3 = List(1, 2, 3)
  private val list1to5 = List(1, 2, 3, 4, 5)

  def applyTest(stream: Stream[Int], expected: List[Int])(f: Stream[Int] => List[Int]): Unit = {
    assert(f(stream) === expected)
  }

  def applyTest(stream: Stream[Int],
                p: Int,
                expected: List[Int])(f: Stream[Int] => Int => Stream[Int]): Unit = {
    assert(f(stream)(p).toList === expected)
  }

  def applyTest(stream: Stream[Int],
                p: Int => Boolean,
                expected: List[Int])(f: Stream[Int] => (Int => Boolean) => Stream[Int]): Unit = {
    assert(f(stream)(p).toList === expected)
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
                    expected: List[Int])(f: Stream[Int] => (=> Int) => Stream[Int]): Unit = {
    assert(f(stream)(p).toList === expected)
  }

  def applyTest(stream: Stream[Int],
                p: Int => Boolean,
                expected: Option[Int])(f: Stream[Int] => (Int => Boolean) => Option[Int]): Unit = {
    assert(f(stream)(p) === expected)
  }

  def applyTest[T](t: T, p: Int, expected: List[T])(f: T => Int => Stream[T]): Unit = {
    assert(f(t)(p).toList === expected)
  }

  def applyTest[S, T](
                       s: Stream[S],
                       t: Stream[T],
                       expected: List[(Option[S], Option[T])]
                     )
                     (f: Stream[S] => Stream[T] => Stream[(Option[S], Option[T])]): Unit = {
    assert(f(s)(t).toList === expected)
  }

  def applyTest[T](s1: Stream[T], s2: Stream[T], expected: Boolean)
                  (f: Stream[T] => Stream[T] => Boolean): Unit = {
    assert(f(s1)(s2) === expected)
  }

  // Renamed as clashes with another applyTest function due to type erasure
  def applyTest2[T](s1: Stream[T], expected: List[List[T]])
                  (f: Stream[T] => Stream[Stream[T]]): Unit = {
    assert(f(s1).map(_.toList).toList === expected)
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
  def runTakeTests(takeTest: (Stream[Int] => Int => Stream[Int]) => Unit): Unit = {
    takeTest({ s: Stream[Int] => s.take })
    takeTest(_.takeFast)
    takeTest(_.takeTextbook)
    takeTest(_.takeViaUnfold)
    takeTest(_.takeViaUnfoldTextbook)
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

  it should "return required number of elements for an infinite stream" in {
    val listOfFive1s = List(1, 1, 1, 1, 1)

    runTakeTests(applyTest(ones, 5, listOfFive1s))
    // Exercise 5.12
    runTakeTests(applyTest(onesViaUnfold, 5, listOfFive1s))
  }

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
  def runTakeWhileTests(takeWhileTest: (Stream[Int] => (Int => Boolean) => Stream[Int]) => Unit): Unit = {
    takeWhileTest({ s: Stream[Int] => s.takeWhile })
    takeWhileTest(_.takeWhileTextbook)
    takeWhileTest(_.takeWhileViaFoldRight)
    takeWhileTest(_.takeWhileViaUnfold)
    takeWhileTest(_.takeWhileViaUnfoldTextbook)
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

  it should "does not return elements infinitely from an infinite stream because it's lazy" in {
    ones.takeWhile(_ == 1)
    // Exercise 5.12
    onesViaUnfold.takeWhile(_ == 1)
  }

  def runExistsTests(existsTest: (Stream[Int] => (Int => Boolean) => Boolean) => Unit): Unit = {
    existsTest({ s: Stream[Int] => s.exists })
    existsTest(_.existsRecursive)
  }

  "exists" should "return true if matching element is present" in {
    runExistsTests(applyTest(stream1to5, _ == 5, expected = true))
  }

  it should "return true if more than one matching element is present" in {
    runExistsTests(applyTest(stream1to5, _ % 2 == 0, expected = true))
  }

  it should "return false if no matching elements are present" in {
    runExistsTests(applyTest(stream1to5, _ % 6 == 0, expected = false))
  }

  it should "return true for infinite stream if the first element satisfies criteria" in {
    runExistsTests(applyTest(ones, _ % 2 == 1, expected = true))
    runExistsTests(applyTest(ones.map(_ + 1), _ % 2 == 0, expected = true))
    // Exercise 5.12
    runExistsTests(applyTest(onesViaUnfold, _ % 2 == 1, expected = true))
    runExistsTests(applyTest(onesViaUnfold.map(_ + 1), _ % 2 == 0, expected = true))
  }

  // Exercise 5.4
  def runForAllTests(forAllTest: (Stream[Int] => (Int => Boolean) => Boolean) => Unit): Unit = {
    forAllTest({ s: Stream[Int] => s.forAll })
    forAllTest(_.forAllRecursive)
  }

  "forAll" should "return true if all elements satisfy criteria" in {
    runForAllTests(applyTest(stream1to5, _ < 6, expected = true))
  }

  it should "return false if some elements do not satisfy criteria" in {
    runForAllTests(applyTest(stream1to5, _ < 5, expected = false))
  }

  it should "return false if all elements do not satisfy criteria" in {
    runForAllTests(applyTest(stream1to5, _ < 0, expected = false))
  }

  it should "return false for an infinite stream if the first element does not satisfy criteria" in {
    runForAllTests(applyTest(ones, _ != 1, expected = false))
    // Exercise 5.12
    runForAllTests(applyTest(onesViaUnfold, _ != 1, expected = false))
  }

  // Exercise 5.6
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

    // Exercise 5.13
    assert(stream1to5.mapViaUnfold(_.toFloat).toList === list1to5Floats)
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

  "appendElem" should "return existing stream with element appended" in {
    applyLazyTest(stream1to5, 6, Range.inclusive(1, 6).toList)(_.appendElem)
  }

  it should "return single element stream when append to empty stream" in {
    applyLazyTest(emptyStream, 1, List(1))(_.appendElem)
  }

  "appendElemIncorrect" should "return existing stream with element appended" in {
    applyLazyTest(stream1to5, 6, Range.inclusive(1, 6).toList)(_.appendElemIncorrect)
  }

  it should "INCORRECTLY return an empty stream when append to empty stream" in {
    applyLazyTest(emptyStream, 1, List())(_.appendElemIncorrect)
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

  def runFindTests(findTest: (Stream[Int] => (Int => Boolean) => Option[Int]) => Unit): Unit = {
    findTest({ s: Stream[Int] => s.find })
    findTest(_.findAlternative)
  }

  "find" should "return element when it matches" in {
    runFindTests(applyTest(stream1to3, _ == 2, Some(2)))
  }

  it should "return first element that matches" in {
    runFindTests(applyTest(stream1to3, _ % 2 == 1, Some(1)))
  }

  it should "not find a match if stream is empty" in {
    runFindTests(applyTest(emptyStream, _ % 2 == 1, None))
  }

  // Exercise 5.8
  def runConstantTests[T](constantTest: (T => Int => Stream[T]) => Unit): Unit = {
    constantTest({ (t: T) => Stream.constant(t).take })
    constantTest({ (t: T) => Stream.constantTextbook(t).take })
    // Exercise 5.12
    constantTest({ (t: T) => Stream.constantViaUnfold(t).take })
  }

  "constant" should "create stream of floats" in {
    runConstantTests(applyTest(2.0, 3, List(2.0, 2.0, 2.0)))
  }

  it should "create stream of strings" in {
    runConstantTests(applyTest("yo", 2, List("yo", "yo")))
  }

  // Exercise 5.9
  def fromTest(s: Int => Stream[Int], n: Int): Unit = {
    s(n).take(3).toList == List(n, n + 1, n + 2)
  }

  "from" should "create stream of consecutive integers" in {
    fromTest(from, 5)
    // Exercise 5.12
    fromTest(fromViaUnfold, 5)
  }

  // Exercise 5.10
  def fibTest(s: Stream[Int]): Unit = {
    s.take(7).toList == List(0, 1, 1, 2, 3, 5, 8)
  }

  "fibs" should "create stream of fibonacci numbers" in {
    fibTest(fibs())
    fibTest(fibsTextbook)
    // Exercise 5.12
    fibTest(fibsViaUnfold)
    fibTest(fibsViaUnfoldVerbose)
  }

  // Exercise 5.11
  def double(initVal: Int = 1): Stream[Int] =
    unfold(initVal)(i => Some(i, 2 * i))

  def doubleTextbook(initVal: Int = 1): Stream[Int] =
    unfoldTextbook(initVal)(i => Some(i, 2 * i))

  def doubleTest(s: Stream[Int]): Unit = {
    s.take(7).toList == List(1, 2, 4, 8, 16, 32, 64)
  }

  "double" should "double every element" in {
    doubleTest(double())
    doubleTest(doubleTextbook())
  }

  // Exercise 5.13
  "zipWith" should "add every pair of numbers together" in {
    assert(stream1to5.zipWith(stream1to5)(_ + _).toList == List(2, 4, 6, 8, 10))
  }

  it should "only add pairs together" in {
    assert(stream1to5.zipWith(stream1to3)(_ + _).toList == List(2, 4, 6))
  }

  "zipTextbook" should "combine every pair of numbers together" in {
    assert(stream1to5.zipTextbook(stream1to5).toList == List((1, 1), (2, 2), (3, 3), (4, 4), (5, 5)))
  }

  it should "only combine pairs together" in {
    assert(stream1to5.zipTextbook(stream1to3).toList == List((1, 1), (2, 2), (3, 3)))
  }

  def runZipAllTests[S, T](zipAllTest: (Stream[S] => Stream[T] => Stream[(Option[S], Option[T])]) => Unit): Unit = {
    zipAllTest(_.zipAll)
    zipAllTest(_.zipAllTextbook)
  }

  "zipAll" should "zip all pairs if streams are same length" in {
    runZipAllTests(
      applyTest(
        Stream("cat", "in", "the", "hat"),
        Stream("green", "eggs", "and", "ham"),
        List(
          (Some("cat"), Some("green")),
          (Some("in"), Some("eggs")),
          (Some("the"), Some("and")),
          (Some("hat"), Some("ham"))
        )
      )
    )
  }

  it should "zip all items if first stream is longer than the second" in {
    runZipAllTests(
      applyTest(
        Stream("cat", "in", "the", "hat"),
        Stream("green", "eggs"),
        List(
          (Some("cat"), Some("green")),
          (Some("in"), Some("eggs")),
          (Some("the"), None),
          (Some("hat"), None)
        )
      )
    )
  }

  it should "zip all items if first stream is shorter than the second" in {
    runZipAllTests(
      applyTest(
        emptyStream,
        Stream("green", "eggs", "and", "ham"),
        List(
          (None, Some("green")),
          (None, Some("eggs")),
          (None, Some("and")),
          (None, Some("ham"))
        )
      )
    )
  }

  it should "return empty stream if both streams are empty" in {
    runZipAllTests(
      applyTest[Int, Int](
        emptyStream, emptyStream, List[(Option[Int], Option[Int])]()
      )
    )
  }

  "zipWithAll" should "process all numbers even if not matched" in {
    assert(stream1to5.zipWithAllTextbook(stream1to3) {
      case (Some(a), Some(b)) => a * b
      case (None, Some(b)) => b * b
      case (Some(a), None) => a * a
      case _ => 0
    }.toList == List(1, 4, 9, 16, 25))
  }

  // Exercise 5.14
  def runStartsWithTests[T](startsWithTest: (Stream[T] => Stream[T] => Boolean) => Unit): Unit = {
    startsWithTest(_.startsWith)
    startsWithTest(_.startsWithTextbook)
  }

  "startsWith" should "return true if stream starts with same elements as supplied stream" in {
    runStartsWithTests(applyTest(stream1to5, stream1to3, expected = true))
  }

  it should "return false if stream does not start with same elements as supplied stream" in {
    runStartsWithTests(applyTest(stream1to3, stream1to5, expected = false))
  }

  it should "return true if supplied stream is empty" in {
    runStartsWithTests(applyTest(stream1to3, emptyStream, expected = true))
  }

  it should "return false if this stream is empty and supplied stream is not empty" in {
    runStartsWithTests(applyTest(emptyStream, stream1to3, expected = false))
  }

  it should "return true if both streams are empty" in {
    runStartsWithTests(applyTest(emptyStream, emptyStream, expected = true))
  }

  // Exercise 5.15
  def runTailsTests[T](tailsTest: (Stream[T] => Stream[Stream[T]]) => Unit): Unit = {
    tailsTest(_.tails)
    tailsTest(_.tailsTextbook)
  }

  "tails" should "return suffixes of input stream" in {
    val expected = List(
      List(1, 2, 3),
      List(2, 3),
      List(3),
      List()
    )
    runTailsTests(applyTest2(stream1to3, expected))
  }

  it should "return empty stream for input empty stream" in {
    runTailsTests(applyTest2(emptyStream, List(emptyList)))
  }

  "hasSubsequence" should "return true if supplied stream occurs in stream" in {
    assert(stream1to3.hasSubsequence(stream1to3))
    assert(stream1to3.hasSubsequence(Stream(1, 2)))
    assert(stream1to3.hasSubsequence(Stream(2, 3)))
    assert(stream1to3.hasSubsequence(Stream(1)))
    assert(stream1to3.hasSubsequence(Stream(2)))
    assert(stream1to3.hasSubsequence(Stream(3)))
    assert(stream1to3.hasSubsequence(emptyStream))
  }

  it should "return false if supplied stream does not occur in stream" in {
    assert(!stream1to3.hasSubsequence(Stream(1, 3)))
  }

  "scanRight" should "work" in {
    assert(stream1to3.scanRightInefficient(0)(_ + _).toList === List(6, 5, 3, 0))
    assert(stream1to3.scanRight(0)(_ + _).toList === List(6, 5, 3, 0))
    assert(stream1to3.scanRightTextbook(0)(_ + _).toList === List(6, 5, 3, 0))
  }
}