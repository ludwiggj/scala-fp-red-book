package fpinscala.applicative

import fpinscala.UnitSpec
import fpinscala.applicative.Applicatives.{streamApplicative, validationApplicative}
import fpinscala.applicative.Validation._
import fpinscala.applicative.Traverse._
import fpinscala.applicative.applicativevsmonad.OptionApplicative
import fpinscala.monoids.Monoid
import org.scalatest.Assertion

import java.util.GregorianCalendar
import scala.language.higherKinds

//noinspection EmptyCheck,OptionEqualsSome
class ApplicativeSpec extends UnitSpec {

  // Exercise 12.3
  "map3" can "combine 3 elements" in {
    assert(OptionApplicative.F.map3(Some(1), Some(2), Some(3))(_ + _ + _) == Some(6))
    assert(OptionApplicative.F.map3(Some(1), None, Some(3))(_ + _ + _) == None)
  }

  "map4" can "combine 4 elements" in {
    assert(OptionApplicative.F.map4(Some("how"), Some("now"), Some("brown"), Some("cow"))(_ + _ + _ + _) ==
      Some("hownowbrowncow"))
    assert(OptionApplicative.F.map4(Some("how"), Some("now"), None, Some("cow"))(_ + _ + _ + _) == None)
  }

  // Exercise 12.4
  "stream applicative" can "sequence lists first element" in {
    val alpha = Stream("a", "b", "c")
    val number = Stream("1", "2", "3", "4")

    val alphaNumerical = streamApplicative.sequence(List(alpha, number))

    assert(alphaNumerical.head == List("a", "1"))
  }

  it can "sequence lists last element" in {
    val alpha = Stream("a", "b", "c")
    val number = Stream("1", "2", "3", "4")

    val alphaNumerical: Stream[List[String]] = streamApplicative.sequence(List(alpha, number))

    assert(alphaNumerical.last == List("c", "3"))
  }

  // Exercise 12.6
  "validationApplicative" can "build a web form" in {
    assert(validWebForm(name = "login", birthdate = "2021-06-21", phone="0147322106") ==
      Success(WebForm("login", new GregorianCalendar(2021, 5, 21).getTime, "0147322106")))
  }

  it can "register a single error when building a web form" in {
    assert(validWebForm(name = "login", birthdate = "2021-06-21", phone="01473221068") ==
      Failure(invalidPhoneNumberMsg, Vector.empty))
  }

  it can "register multiple errors when building a web form" in {
    assert(validWebForm(name = "", birthdate = "this_is_not_a_date", phone="01473221068") ==
      Failure(invalidNameMsg, Vector(invalidBirthdateMsg, invalidPhoneNumberMsg)))
  }

  // Exercise 12.8
  "product" can "combine applicative functors" in {
    assert(validationApplicative.product(OptionApplicative.F).unit("Hello") ==
      ((Success("Hello"), Some("Hello"))))
  }

  // Exercise 12.9
  "compose" can "combine applicative functors" in {
    assert(validationApplicative.compose(OptionApplicative.F).unit("Hello") == Success(Some("Hello")))

    // TODO - This doesn't compile
    // assert(OptionApplicative.F.compose(validationApplicative).unit("Hello") == Some(Success("Hello")))
  }

  // Exercise 12.12
  "option applicative" can "sequence a map and return a result" in {
    assert(OptionApplicative.F.sequenceMap[String, Int](
      Map("one" -> Some(1), "two" -> Some(2), "three" -> Some(3))
    ) == Some(Map("one" -> 1, "two" -> 2, "three" -> 3)))

    assert(OptionApplicative.F.sequenceMapTextbook[String, Int](
      Map("one" -> Some(1), "two" -> Some(2), "three" -> Some(3))
    ) == Some(Map("one" -> 1, "two" -> 2, "three" -> 3)))
  }

  it can "sequence a map and return an empty result" in {
    assert(OptionApplicative.F.sequenceMap[String, Int](
      Map("one" -> Some(1), "two" -> None, "three" -> Some(3))
    ) == None)

    assert(OptionApplicative.F.sequenceMapTextbook[String, Int](
      Map("one" -> Some(1), "two" -> None, "three" -> Some(3))
    ) == None)
  }

  // Exercise 12.13
  "traversible" can "traverse list and convert to stream" in {
    // Uses stream applicative
    val result = listTraverse.traverse(List(1, 2, 3))(Applicative[Stream].unit(_))
    assert(result == Stream(List(1, 2, 3)))
  }

  it can "traverse list and convert to validation success" in {
    implicit val applicative: Applicative[Success] = Applicatives.validationSuccessApplicative

    val result = listTraverse.traverse(List(1, 2, 3))(applicative.unit(_))
    assert(result == Success(List(1, 2, 3)))
  }

  it can "traverse list and convert to validation if one type is fixed" in {
    type MyValidation[T] = Validation[String, T]

    implicit val applicative: Applicative[MyValidation] = validationApplicative

    val result = listTraverse.traverse(List(1, 2, 3))(applicative.unit(_))

    assert(result == Success(List(1, 2, 3)))
  }

  it can "traverse option (Some)" in {
    // Uses stream applicative
    val result = optionTraverse.traverse(Some("Hi"))(Applicative[Stream].unit(_))
    assert(result == Stream(Some("Hi")))

    val resultTextbook = optionTraverseTextbook.traverse(Some("Hi"))(Applicative[Stream].unit(_))
    assert(resultTextbook == Stream(Some("Hi")))
  }

  it can "traverse option (None)" in {
    // Uses stream applicative
    val result = optionTraverse.traverse(None: Option[String])(Applicative[Stream].unit(_))
    assert(result == Stream(None))

    val resultTextbook = optionTraverseTextbook.traverse(None: Option[String])(Applicative[Stream].unit(_))
    assert(resultTextbook == Stream(None))
  }

  it can "traverse tree" in {
    // Uses stream applicative
    val tree = Tree(1, List(Tree(2, List()), Tree(3, List(Tree(4, List())))))
    val result = treeTraverse.traverse(tree)(Applicative[Stream].unit(_))
    assert(result == Stream(tree))
  }

  // Exercise 12.14
  it can "map" in {
    assert(listTraverse.map(List(1, 2, 3))(_.toString) == List("1", "2", "3"))
    assert(listTraverse._map(List(1, 2, 3))(_.toString) == List("1", "2", "3"))
  }

  it can "foldMap" in {
    assert(listTraverse.foldMap(List("1", "3", "5", "7", "9"))(_.toInt)(Monoid.intAddition) == 25)
    assert(listTraverse._foldMap(List("1", "3", "5", "7", "9"))(_.toInt)(Monoid.intAddition) == 25)
  }

  "zip with index" can "work for list" in {
    assert(listTraverse._zipWithIndex(List("A", "B", "C")) == List(("A", 0), ("B", 1), ("C", 2)))
    assert(listTraverse.zipWithIndex(List("A", "B", "C")) == List(("A", 0), ("B", 1), ("C", 2)))
  }

  it can "work for option (Some)" in {
    //noinspection OptionEqualsSome
    assert(optionTraverse._zipWithIndex(Some("A")) == Some(("A", 0)))
    assert(optionTraverse.zipWithIndex(Some("A")) == Some(("A", 0)))
  }

  it can "work for option (None)" in {
    assert(optionTraverse._zipWithIndex(None).isEmpty)
    assert(optionTraverse.zipWithIndex(None).isEmpty)
  }

  it can "work for tree" in {
    assert(treeTraverse._zipWithIndex(Tree(1, List(Tree(2, List()), Tree(3, List(Tree(4, List())))))) ==
      Tree((1, 0), List(Tree((2, 1), List()), Tree((3, 2), List(Tree((4, 3), List())))))
    )
    assert(treeTraverse.zipWithIndex(Tree(1, List(Tree(2, List()), Tree(3, List(Tree(4, List())))))) ==
      Tree((1, 0), List(Tree((2, 1), List()), Tree((3, 2), List(Tree((4, 3), List())))))
    )
  }

  "zip to list" can "zip a list" in {
    assert(listTraverse._toList(List("A", "B", "C")) == List("A", "B", "C"))
    assert(listTraverse.toList(List("A", "B", "C")) == List("A", "B", "C"))
  }

  it can "zip an option (Some)" in {
    assert(optionTraverse._toList(Some("A")) == List("A"))
    assert(optionTraverse.toList(Some("A")) == List("A"))
  }

  it can "zip an option (None)" in {
    assert(optionTraverse._toList(None) == List())
    assert(optionTraverse.toList(None) == List())
  }

  it can "zip a tree" in {
    assert(treeTraverse._toList(Tree(1, List(Tree(2, List()), Tree(3, List(Tree(4, List())))))) == List(1, 2, 3, 4))
    assert(treeTraverse.toList(Tree(1, List(Tree(2, List()), Tree(3, List(Tree(4, List())))))) == List(1, 2, 3, 4))
  }

  // Exercise 12.16: Check that following law is obeyed:

  // toList(reverse(x)) ++ toList(reverse(y)) == reverse(toList(y) ++ toList(x))
  def validateTraverseReverseLaw[F[_], A](traverse: Traverse[F], x: F[A], y: F[A]): Assertion = {
    val left: List[A] = traverse.toList(traverse.reverse(x)) ++ traverse.toList(traverse.reverse(y))

    val joinedList: List[A] = traverse.toList(y) ++ traverse.toList(x)

    // We use listTraverse here, as we're operating on a list
    val right: List[A] = listTraverse.reverse(joinedList)

    assert(left == right)
  }

  "traverse reverse law" can "be satisfied by list traversal" in {
    validateTraverseReverseLaw(listTraverse, List(1, 2, 3), List(5))
  }

  it can "be satisfied by option traversal" in {
    validateTraverseReverseLaw(optionTraverse, Some(1), Some(2))
    validateTraverseReverseLaw(optionTraverse, None, Some(2))
    validateTraverseReverseLaw(optionTraverse, Some("A"), None)
    validateTraverseReverseLaw(optionTraverse, None, None)
  }

  it can "be satisfied by tree traversal" in {
    validateTraverseReverseLaw(
      treeTraverse,
      Tree(1, List(Tree(2, List()))), Tree(3, List(Tree(4, List()), Tree(5, List())))
    )
  }

  "reverse" can "reverse a list" in {
    assert(listTraverse.reverse(List("A", "B", "C")) == List("C", "B", "A"))
  }

  it can "reverse an option (Some)" in {
    //noinspection OptionEqualsSome
    assert(optionTraverse.reverse(Some("1")) == Some("1"))
  }

  it can "reverse an option (None)" in {
    assert(optionTraverse.reverse(None) == None)
  }

  it can "reverse a tree" in {
    assert(treeTraverse.reverse(
      Tree(1, List(Tree(2, List(Tree(3, List()))), Tree(4, List(Tree(5, List())))))
    ) ==
      Tree(5, List(Tree(4, List(Tree(3, List()))), Tree(2, List(Tree(1, List())))))
    )
  }

  "foldLeft" can "fold a list" in {
    assert(listTraverse._foldLeft(List("D", "o", "g"))("A")(_ + _) == (List("AD", "ADo", "ADog"), "ADog"))
    assert(listTraverse.foldLeft(List("D", "o", "g"))("A")(_ + _) == "ADog")
  }

  it can "fold an option (Some)" in {
    assert(optionTraverse._foldLeft(Some(5))("A")(_ * _) == (Some("AAAAA"), "AAAAA"))
    assert(optionTraverse.foldLeft(Some(5))("A")(_ * _) == "AAAAA")
  }

  it can "fold an option (None)" in {
    assert(optionTraverse._foldLeft(None)("A")(_ * _) == (None, "A"))
    assert(optionTraverse.foldLeft(None)("A")(_ * _) == "A")
  }

  it can "fold a tree" in {
    val t = Tree(1, List(Tree(2, List(Tree(3, List()))), Tree(4, List(Tree(5, List())))))
    val expectedTree = Tree(3, List(Tree(6, List(Tree(18, List()))), Tree(72, List(Tree(360, List())))))

    assert(treeTraverse._foldLeft(t)(3)(_ * _) == (expectedTree, 360))
    assert(treeTraverse.foldLeft(t)(3)(_ * _) == 360)
  }

  "zip" can "join lists of same length" in {
    assert(listTraverse.zip(List(1, 2, 3), List(4, 5, 6)) == List((1, 4), (2, 5), (3, 6)))
  }

  it can "join lists where second list is longer" in {
    assert(listTraverse.zip(List(1, 2, 3), List(4, 5, 6, 7)) == List((1, 4), (2, 5), (3, 6)))
  }

  it can "fail to join lists where second list is shorter" in {
    val caught =
      intercept[RuntimeException] { // Result type: IndexOutOfBoundsException
        listTraverse.zip(List(1, 2, 3), List(4, 5))
      }
    assert(caught.getMessage == "zip: Incompatible shapes.")
  }

  it can "join trees where branches are of same length" in {
    val t = Tree(1, List(Tree(2, List(Tree(3, List(Tree(4, List(Tree(5, List())))))))))

    assert(treeTraverse.zip(t, t) ==
      Tree((1, 1), List(Tree((2, 2), List(Tree((3, 3), List(Tree((4, 4), List(Tree((5, 5), List())))))))))
    )
  }

  it can "join trees where second tree branches are of same length or longer" in {
    val t1 = Tree(1, List(Tree(2, List(Tree(3, List(Tree(4, List(Tree(5, List())))))))))
    val t2 = Tree(5, List(Tree(4, List(Tree(3, List(Tree(2, List(Tree(1, List()))))))), Tree(6, List())))

    assert(treeTraverse.zip(t1, t2) ==
      Tree((1, 5), List(Tree((2, 4), List(Tree((3, 3), List(Tree((4, 2), List(Tree((5, 1), List())))))))))
    )
  }
  
  it can "fail to join trees where second tree branch is shorter" in {
    val t1 = Tree(5, List(Tree(4, List(Tree(3, List(Tree(2, List(Tree(1, List()))))))), Tree(6, List())))
    val t2 = Tree(1, List(Tree(2, List(Tree(3, List(Tree(4, List(Tree(5, List())))))))))

    val caught =
      intercept[RuntimeException] { // Result type: IndexOutOfBoundsException
        treeTraverse.zip(t1, t2)
      }
    assert(caught.getMessage == "zip: Incompatible shapes.")
  }

  // zipL result has shape of left argument i.e. 3 elements
  "zipL" can "join lists of same length" in {
    assert(listTraverse.zipL(List(1, 2, 3), List(4, 5, 6)) == List((1, Some(4)), (2, Some(5)), (3, Some(6))))
  }

  it can "join lists where second list is longer" in {
    assert(listTraverse.zipL(List(1, 2, 3), List(4, 5, 6, 7)) == List((1, Some(4)), (2, Some(5)), (3, Some(6))))
  }

  it can "join lists where second list is shorter" in {
    assert(listTraverse.zipL(List(1, 2, 3), List(4, 5)) == List((1, Some(4)), (2, Some(5)), (3, None)))
  }

  // zipR result has shape of right argument
  "zipR" can "join lists of same length" in {
    assert(listTraverse.zipR(List(1, 2, 3), List(4, 5, 6)) == List((Some(1), 4), (Some(2), 5), (Some(3), 6)))
  }

  it can "join lists where second list is longer" in {
    assert(listTraverse.zipR(List(1, 2, 3), List(4, 5, 6, 7)) == List((Some(1), 4), (Some(2), 5), (Some(3), 6), (None, 7)))
  }

  it can "join lists where second list is shorter" in {
    assert(listTraverse.zipR(List(1, 2, 3), List(4, 5)) == List((Some(1), 4), (Some(2), 5)))
  }

  import fpinscala.applicative.applicativevsmonad.OptionApplicative.F

  "fuse" can "combine two traverses into one" in {
    implicit val optionApplicative: Applicative[Option] = F

    assert(listTraverse.fuse(List(1, 2, 3, 4))(i => Option(i * 2), i => Stream(i * 2)) ==
      (Some(List(2, 4, 6, 8)), Stream(List(2, 4, 6, 8)))
    )
  }

  it can "combine two different traverses into one" in {
    implicit val optionApplicative: Applicative[Option] = F

    type MyValidation[T] = Validation[String, T]
    implicit val applicative: Applicative[MyValidation] = validationApplicative

    assert(listTraverse.fuse(List(1, 2, 3, 4))(i => Option(i * 2), i => Validation.success(i * 2): MyValidation[Int]) ==
      (Some(List(2, 4, 6, 8)), Success(List(2, 4, 6, 8)))
    )
  }

  "compose" can "compose traverses" in {
    implicit val optTraverse: Traverse[Option] = optionTraverse

    val listOptionTraverse = listTraverse.compose

    assert(listOptionTraverse.traverse(List(Option(1), Some(2), None))(Applicative[Stream].unit(_))
      == Stream(List(Option(1), Some(2), None)))
  }
}