package fpinscala.laziness

import fpinscala.UnitSpec

class StreamSpec extends UnitSpec {
  private val emptyStream = Stream[Int]()
  private val emptyList = List[Int]()
  private val stream123 = Stream(1, 2, 3)
  private val list123 = List(1, 2, 3)

  "headOption" should "return None if stream is empty" in {
    assert(emptyStream.headOption === None)
  }

  it should "return first element if stream is not empty" in {
    assert(stream123.headOption === Some(1))
  }

  // Exercise 5.1
  def toListTest(stream: Stream[Int], expectedList: List[Int])(toList: Stream[Int] => List[Int]): Unit = {
    assert(toList(stream) === expectedList)
  }

  "toList" should "return empty list if stream is empty" in {
    def emptyStreamToListTest: (Stream[Int] => List[Int]) => Unit = toListTest(emptyStream, emptyList)

    // Calling multiple implementations of same method in same test
    emptyStreamToListTest(_.toListRecursiveTextbook)
    emptyStreamToListTest(_.toListRecursive)
    emptyStreamToListTest(_.toList)
    emptyStreamToListTest(_.toListFast)
    emptyStreamToListTest(_.toListViaFoldRight)
  }

  it should "return list containing same elements in the same order" in {
    def nonEmptyStreamToListTest: (Stream[Int] => List[Int]) => Unit = toListTest(stream123, list123)

    // Calling multiple implementations of same method in same test
    nonEmptyStreamToListTest(_.toListRecursiveTextbook)
    nonEmptyStreamToListTest(_.toListRecursive)
    nonEmptyStreamToListTest(_.toList)
    nonEmptyStreamToListTest(_.toListFast)
    nonEmptyStreamToListTest(_.toListViaFoldRight)
  }
}