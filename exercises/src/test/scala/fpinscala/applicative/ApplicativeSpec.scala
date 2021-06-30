package fpinscala.applicative

import com.typesafe.scalalogging.LazyLogging
import fpinscala.UnitSpec
import fpinscala.applicative.Applicative.Streaming.streamApplicative
import fpinscala.applicative.Applicative.Validation._
import fpinscala.applicative.Traverse.{listTraverse, optionTraverse, optionTraverseTextbook}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import java.util.GregorianCalendar

class ApplicativeSpec extends UnitSpec with ScalaCheckPropertyChecks with LazyLogging {

  "stream applicative" can "sequence lists first element" in {
    val alpha = Stream("a", "b", "c")
    val number = Stream("1", "2", "3", "4")

    val alphaNumerical = streamApplicative.sequence(List(alpha, number))

    assert(alphaNumerical.head == List("a", "1"))
  }

  "it" can "sequence lists last element" in {
    val alpha = Stream("a", "b", "c")
    val number = Stream("1", "2", "3", "4")

    val alphaNumerical: Stream[List[String]] = streamApplicative.sequence(List(alpha, number))

    assert(alphaNumerical.last == List("c", "3"))
  }

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

  "traversible" can "work for list" in {
    // Uses stream applicative
    val result = listTraverse.traverse(List(1, 2, 3))(implicitly[Applicative[Stream]].unit(_))
    assert(result == Stream(List(1, 2, 3)))
  }

  it can "work for option (Some)" in {
    // Uses stream applicative
    val result = optionTraverse.traverse(Some("Hi"))(implicitly[Applicative[Stream]].unit(_))
    assert(result == Stream(Some("Hi")))

    val resultTextbook = optionTraverseTextbook.traverse(Some("Hi"))(implicitly[Applicative[Stream]].unit(_))
    assert(resultTextbook == Stream(Some("Hi")))
  }

  it can "work for option (None)" in {
    // Uses stream applicative
    val result = optionTraverse.traverse(None: Option[String])(implicitly[Applicative[Stream]].unit(_))
    assert(result == Stream(None))

    val resultTextbook = optionTraverseTextbook.traverse(None: Option[String])(implicitly[Applicative[Stream]].unit(_))
    assert(resultTextbook == Stream(None))
  }
}