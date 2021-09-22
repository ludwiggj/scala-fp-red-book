package fpinscala.monoids

import com.typesafe.scalalogging.LazyLogging
import fpinscala.UnitSpec
import fpinscala.monoids.Monoid._
import fpinscala.parallelism.nonblocking.Par
import fpinscala.state.{RNG, State}
import fpinscala.testing.take2.{Gen, Passed, Prop}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import java.util.concurrent.Executors

class MonoidSpec extends UnitSpec with ScalaCheckPropertyChecks with LazyLogging {

  "fold a tree" can "multiply all nodes" in {
    val tree = Branch(Branch(Leaf(1), Branch(Leaf(3), Leaf(5))), Leaf(6))
    assert(TreeFoldable.foldLeft(tree)(1)(_ * _) == 90)
  }

  it can "add all nodes" in {
    val tree = Branch(Branch(Leaf(1), Branch(Leaf(3), Leaf(5))), Leaf(6))
    assert(TreeFoldable.foldRight(tree)(0)(_ + _) == 15)
  }

  it can "add all nodes in string tree using int monoid" in {
    val tree = Branch(Branch(Leaf("1"), Branch(Leaf("3"), Leaf("5"))), Leaf("6"))
    assert(TreeFoldable.foldMap(tree)(_.toInt)(intAddition) == 15)
  }

  "foldable toList" should "return list for ListFoldable" in {
    assert(ListFoldable.toList(List(1, 2, 3)) == List(1, 2, 3))
  }

  it should "return list for IndexedSeqFoldable" in {
    assert(IndexedSeqFoldable.toList(IndexedSeq(1, 2, 3)) == List(1, 2, 3))
  }

  it should "return list for StreamFoldable" in {
    assert(StreamFoldable.toList(Stream(1, 2, 3)) == List(1, 2, 3))
  }

  it should "return list for TreeFoldable" in {
    val tree = Branch(Branch(Leaf(1), Branch(Leaf(3), Leaf(5))), Leaf(6))
    assert(TreeFoldable.toList(tree) == List(1, 3, 5, 6))
  }

  it should "return list for OptionFoldable with Some" in {
    assert(OptionFoldable.toList(Some(3)) == List(3))
  }

  it should "return list for OptionFoldable with None" in {
    assert(OptionFoldable.toList(None) == List())
  }

  "stringMonoid" should "concatenate strings" in {
    val input = List("The ", "cat ", "sat ", "on ", "the ", "mat ")
    val expectedMessage = "The cat sat on the mat "

    assert(concatenate(input, stringMonoid) == expectedMessage)
  }

  "stringMonoid dual" should "concatenate strings in reverse order" in {
    val input = List("The ", "cat ", "sat ", "on ", "the ", "mat ")
    val expectedMessage = "mat the on sat cat The "

    assert(concatenate(input, dual(stringMonoid)) == expectedMessage)
  }

  "listMonoid" should "concatenate list elements" in {
    val input = List(List("The ", "cat ", "sat "), List("on ", "the ", "mat"))
    val expectedList = List("The ", "cat ", "sat ", "on ", "the ", "mat")

    assert(concatenate(input, listMonoid[String]) == expectedList)
  }

  "listMonoid dual" should "concatenate list elements in reverse order" in {
    val input = List(List("The ", "cat ", "sat "), List("on ", "the ", "mat"))
    val expectedList = List("on ", "the ", "mat", "The ", "cat ", "sat ")

    assert(concatenate(input, dual(listMonoid[String])) == expectedList)
  }

  "intAdditionMonoid" should "add a list of numbers" in {
    val input = List(1, 2, 3, 4, 5)
    val expectedSum = 15

    assert(concatenate(input, intAddition) == expectedSum)
  }

  "intAdditionMonoid dual" should "add a list of numbers" in {
    val input = List(1, 2, 3, 4, 5)
    val expectedSum = 15

    assert(concatenate(input, dual(intAddition)) == expectedSum)
  }

  "intMultiplicationMonoid" should "multiply a list of numbers" in {
    val input = List(1, 2, 3, 4, 5)
    val expectedProduct = 120

    assert(concatenate(input, intMultiplication) == expectedProduct)
  }

  "intMultiplicationMonoid dual" should "multiply a list of numbers" in {
    val input = List(1, 2, 3, 4, 5)
    val expectedProduct = 120

    assert(concatenate(input, dual(intMultiplication)) == expectedProduct)
  }

  "booleanOrMonoid" should "return true if any of elements is true" in {
    val input = List(false, false, true)
    val expectedResult = true

    assert(concatenate(input, booleanOr) == expectedResult)
  }

  it should "return false if all elements are false" in {
    val input = List(false, false, false)
    val expectedResult = false

    assert(concatenate(input, booleanOr) == expectedResult)
  }

  "booleanOrMonoid dual" should "return true if any of elements is true" in {
    val input = List(false, false, true)
    val expectedResult = true

    assert(concatenate(input, dual(booleanOr)) == expectedResult)
  }

  it should "return false if all elements are false" in {
    val input = List(false, false, false)
    val expectedResult = false

    assert(concatenate(input, dual(booleanOr)) == expectedResult)
  }

  "booleanAndMonoid" should "return false if any of elements is false" in {
    val input = List(true, false, true)
    val expectedResult = false

    assert(concatenate(input, booleanAnd) == expectedResult)
  }

  it should "return true if all elements are true" in {
    val input = List(true, true, true)
    val expectedResult = true

    assert(concatenate(input, booleanAnd) == expectedResult)
  }

  "booleanAndMonoid dual" should "return false if any of elements is false" in {
    val input = List(true, false, true)
    val expectedResult = false

    assert(concatenate(input, dual(booleanAnd)) == expectedResult)
  }

  it should "return true if all elements are true" in {
    val input = List(true, true, true)
    val expectedResult = true

    assert(concatenate(input, dual(booleanAnd)) == expectedResult)
  }

  "optionMonoid" should "return first element if it is a Some" in {
    val input: List[Option[Int]] = List(Some(1), Some(2), Some(3))
    val expectedResult = Some(1)

    assert(concatenate(input, optionMonoid[Int]) == expectedResult)
  }

  it should "return first element that is a Some" in {
    val input: List[Option[Int]] = List(None, Some(2), Some(3))
    val expectedResult = Some(2)

    assert(concatenate(input, optionMonoid[Int]) == expectedResult)
  }

  it should "return None if all elements are None" in {
    val input: List[Option[Int]] = List(None, None, None)
    val expectedResult = None

    assert(concatenate(input, optionMonoid[Int]) == expectedResult)
  }

  "optionMonoid dual" should "return last element if it is a Some" in {
    val input: List[Option[Int]] = List(Some(1), Some(2), Some(3))
    val expectedResult = Some(3)

    assert(concatenate(input, dual(optionMonoid[Int])) == expectedResult)
  }

  it should "return first element from end that is a Some" in {
    val input: List[Option[Int]] = List(Some(1), Some(2), None)
    val expectedResult = Some(2)

    assert(concatenate(input, dual(optionMonoid[Int])) == expectedResult)
  }

  it should "return None if all elements are None" in {
    val input: List[Option[Int]] = List(None, None, None)
    val expectedResult = None

    assert(concatenate(input, dual(optionMonoid[Int])) == expectedResult)
  }

  "endoMonoid" should "concatenate operations" in {
    val intOps: List[Int => Int] = List(i => i * 2, i => i * 3, i => i / 4)
    val expectedResult = 6

    assert(concatenate(intOps, endoMonoid[Int])(4) == expectedResult)
  }

  "endoMonoid dual" should "concatenate operations" in {
    val intOps: List[Int => Int] = List(i => i * 2, i => i * 3, i => i / 4)
    val expectedResult = 6

    assert(concatenate(intOps, dual(endoMonoid[Int]))(4) == expectedResult)
  }

  "monoid laws" should "hold for string" in {
    val prop = monoidLaws(stringMonoid, Gen.stringGen(10))

    assert(Prop.run(prop, maxSize = 10, testCases = 30) == Passed)
  }

  it should "hold for list" in {
    val prop = monoidLaws[List[String]](listMonoid[String], Gen.listOfN(5, Gen.stringGen(10)))

    assert(Prop.run(prop, maxSize = 10, testCases = 30) == Passed)
  }

  it should "hold for int addition" in {
    val prop = monoidLaws(intAddition, Gen.choose(1, 100))

    assert(Prop.run(prop, maxSize = 10, testCases = 30) == Passed)
  }

  it should "hold for int multiplication" in {
    val prop = monoidLaws(intMultiplication, Gen.choose(1, 100))

    assert(Prop.run(prop, maxSize = 10, testCases = 30) == Passed)
  }

  it should "hold for boolean or" in {
    val prop = monoidLaws(booleanOr, Gen(State(RNG.boolean)))

    assert(Prop.run(prop, maxSize = 10, testCases = 30) == Passed)
  }

  it should "hold for boolean and" in {
    val prop = monoidLaws(booleanAnd, Gen(State(RNG.boolean)))

    assert(Prop.run(prop, maxSize = 10, testCases = 30) == Passed)
  }

  it should "hold for option" in {
    val prop = monoidLaws[Option[Int]](optionMonoid, Gen.toOption(Gen.choose(1, 100)))

    assert(Prop.run(prop, maxSize = 10, testCases = 30) == Passed)
  }

  it should "hold for WC" in {
    testWCMonoidLaws(wcMonoid)
  }

  it should "hold for WC textbook" in {
    testWCMonoidLaws(wcMonoidTextbook)
  }

  "foldMap" should "add stringy numbers using int monoid" in {
    val input = List("1", "2", "3", "4", "5")
    val expectedSum = 15

    assert(foldMap(input, intAddition)(_.toInt) == expectedSum)
  }

  "foldMapTextbook" should "add stringy numbers using int monoid" in {
    val input = List("1", "2", "3", "4", "5")
    val expectedSum = 15

    assert(foldMapTextbook(input, intAddition)(_.toInt) == expectedSum)
  }

  "foldRight" should "calculate correct result" in {
    val numbers = List(1, 2, 3, 4, 5)

    // 1 - (2 - (3 - (4 - (5 - 0))))
    assert(foldRight(numbers)(0)(_ - _) == 3)
  }

  "foldLeft" should "calculate correct result" in {
    val numbers = List(1, 2, 3, 4, 5)

    // 0 - (1 - (2 - (3 - (4 - 5))))
    assert(foldLeft(numbers)(0)(_ - _) == -15)
  }

  "foldMapV" should "add stringy numbers using int monoid, odd list length" in {
    val input = IndexedSeq("1", "2", "3", "4", "5")
    val expectedSum = 15

    assert(foldMapV(input, intAddition)(_.toInt) == expectedSum)
  }

  it should "add stringy numbers using int monoid, even list length" in {
    val input = IndexedSeq("1", "2", "3", "4", "5", "6")
    val expectedSum = 21

    assert(foldMapV(input, intAddition)(_.toInt) == expectedSum)
  }

  it should "add stringy numbers using int monoid, single element list" in {
    val input = IndexedSeq("1")
    val expectedSum = 1

    assert(foldMapV(input, intAddition)(_.toInt) == expectedSum)
  }

  it should "add stringy numbers using int monoid, empty list" in {
    val input = IndexedSeq[String]()
    val expectedSum = 0

    assert(foldMapV(input, intAddition)(_.toInt) == expectedSum)
  }

  "parFoldMap" should "add stringy numbers using int monoid" in {
    val service = Executors.newFixedThreadPool(2)
    val input = IndexedSeq("1", "2", "3", "4", "5", "6")
    val expectedSum = 21

    assert(Par.run(service)(parFoldMap(input, intAddition)(_.toInt)) == expectedSum)
  }

  "ordered" should "identify collection in ascending order" in {
    assert(ordered(IndexedSeq(1, 2, 3)))
  }

  it should "identify collection not in ascending order" in {
    assert(!ordered(IndexedSeq(1, 3, 2)))
  }

  private def testWCMonoidLaws(monoid: Monoid[WC]): Any = {
    val chars = ('0' to '9').toList ++ ('a' to 'z').toList ++ ('A' to 'Z').toList

    def randomStringGen: Gen[String] = Gen.choose(1, 10).flatMap(l => Gen.stringGen(l, chars))

    val stubGen: Gen[Stub] = randomStringGen.map(Stub)

    val partGen: Gen[Part] = for {
      lStub <- randomStringGen
      words <- Gen.choose(1, 20)
      rStub <- randomStringGen
    } yield Part(lStub, words, rStub)

    val wcGen: Gen[WC] = Prop.weighted((stubGen, 0.5), (partGen, 0.5))

    val prop = monoidLaws(monoid, wcGen)

    assert(Prop.run(prop, maxSize = 10, testCases = 30) == Passed)
  }

  "count" should "return 0 for an empty string" in {
    assert(count("") == 0)
  }

  it should "return 1 for a single word" in {
    assert(count("hello") == 1)
  }

  it should "return 2 for two words" in {
    assert(count("hello dolly") == 2)
  }

  it should "return 2 for two unbalanced words" in {
    assert(count("I believe") == 2)
  }

  it should "count words in paragraph" in {
    val input = "The spot was perfect for camouflage. At least that's what she thought when she picked the spot. " +
      "She couldn't imagine that anyone would ever be able to see her in these surroundings. So there she sat, " +
      "confident that she was hidden from the world and safe from danger. Unfortunately, she had not anticipated " +
      "that others may be looking upon her from other angles, and now they were stealthily descending toward her " +
      "hiding spot."

    // Here's the breakdown...

    // The spot was perfect for camouflage. At least that's   = The 8 -
    // what she thought when she picked the spot. She couldn  = what 8 couldn
    //                                                        > The 17 couldn
    //
    // 't imagine that anyone would ever be able to see her   = 't 10 -
    // in these surroundings. So there she sat, confident th  = in 7 th
    //                                                        > 't 18 th
    //
    //                                                        >> The 36 th
    //
    // at she was hidden from the world and safe from danger  = at 9 danger
    // . Unfortunately, she had not anticipated that others   = . 7 -
    //                                                        > at 17 -
    //
    // may be looking upon her from other angles, and now th  = may 9 th
    // ey were stealthily descending toward her hiding spot.  = ey 6 spot.
    //                                                        > may 16 spot.
    //
    //                                                        >> at 34 spot.
    //
    //                                                        >>> The 71 spot.

    assert(count(input) == 73)
  }

  "countTextbook" should "return 0 for an empty string" in {
    assert(countTextbook("") == 0)
  }

  it should "return 1 for a single word" in {
    assert(countTextbook("hello") == 1)
  }

  it should "return 2 for two words" in {
    assert(countTextbook("hello dolly") == 2)
  }

  it should "return 2 for two unbalanced words" in {
    assert(countTextbook("I believe") == 2)
  }

  it should "count words in paragraph" in {
    val input = "The spot was perfect for camouflage. At least that's what she thought when she picked the spot. " +
      "She couldn't imagine that anyone would ever be able to see her in these surroundings. So there she sat, " +
      "confident that she was hidden from the world and safe from danger. Unfortunately, she had not anticipated " +
      "that others may be looking upon her from other angles, and now they were stealthily descending toward her " +
      "hiding spot."

    assert(countTextbook(input) == 73)
  }

  "foldable option" should "foldMap on Some" in {
    assert(OptionFoldable.foldMap(Some("5"))(_.toInt)(intMultiplication) == 5)
  }

  it should "foldMap on None" in {
    assert(OptionFoldable.foldMap[String, Int](None)(_.toInt)(intMultiplication) == 1)
  }

  it should "foldLeft on Some" in {
    assert(OptionFoldable.foldLeft(Some(5))(2)(_ * _) == 10)
  }

  it should "foldLeft on None" in {
    assert(OptionFoldable.foldLeft[Int, Int](None)(2)(_ * _) == 2)
  }

  it should "foldRight on Some" in {
    assert(OptionFoldable.foldRight(Some(5))(2)(_ * _) == 10)
  }

  it should "foldRight on None" in {
    assert(OptionFoldable.foldRight[Int, Int](None)(2)(_ * _) == 2)
  }

  "textbook foldable option" should "foldMap on Some" in {
    assert(OptionFoldableTextbook.foldMap(Some("5"))(_.toInt)(intMultiplication) == 5)
  }

  it should "foldMap on None" in {
    assert(OptionFoldableTextbook.foldMap[String, Int](None)(_.toInt)(intMultiplication) == 1)
  }

  it should "foldLeft on Some" in {
    assert(OptionFoldableTextbook.foldLeft(Some(5))(2)(_ * _) == 10)
  }

  it should "foldLeft on None" in {
    assert(OptionFoldableTextbook.foldLeft[Int, Int](None)(2)(_ * _) == 2)
  }

  it should "foldRight on Some" in {
    assert(OptionFoldableTextbook.foldRight(Some(5))(2)(_ * _) == 10)
  }

  it should "foldRight on None" in {
    assert(OptionFoldableTextbook.foldRight[Int, Int](None)(2)(_ * _) == 2)
  }

  "mapMergeMonoid" should "merge maps" in {
    val N: Monoid[Map[String, Int]] = mapMergeMonoid(intAddition)
    val M: Monoid[Map[String, Map[String, Int]]] = mapMergeMonoid(mapMergeMonoid(intAddition))

    val m1 = Map("o1" -> Map("i1" -> 1, "i2" -> 2))
    val m2 = Map("o1" -> Map("i2" -> 3))

    assert(M.op(m1, m2) == Map("o1" -> Map("i1" -> 1, "i2" -> 5)))
  }

  "bag" should "calculate bag from indexed sequence" in {
    assert(bag(Vector("a", "rose", "is", "a", "rose")) == Map("a" -> 2, "rose" -> 2, "is" -> 1))
  }

  "product monoid" should "allow us to calculate mean of list" in {
    // Compose monoids to calculate sum and length of list of numbers at the same time
    val m: Monoid[(Int, Int)] = productMonoid(intAddition, intAddition)

    val p: (Int, Int) = ListFoldable.foldMap(List(1, 2, 3, 4))(a => (1, a))(m)

    assert(p._2 / p._1.toDouble == 2.5)
  }
}