package fpinscala.applicative

import java.util.Date

// Exercise 12.6

// Validation is a new data type that is much like Either
// except that it can explicitly handle more than one error
sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E])
  extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]

case class WebForm(name: String, birthdate: Date, phoneNumber: String)

object Validation {
  val invalidNameMsg = "Name cannot be empty"
  val invalidBirthdateMsg = "Birthdate must be in the form yyyy-MM-dd"
  val invalidPhoneNumberMsg = "Phone number must be 10 digits"

  def validName(name: String): Validation[String, String] =
    if (name != "")
      Success(name)
    else
      Failure(invalidNameMsg, Vector.empty)

  def validBirthdate(birthdate: String): Validation[String, Date] = {
    try {
      import java.text._
      Success(new SimpleDateFormat("yyyy-MM-dd").parse(birthdate))
    }
    catch {
      case _: Exception => Failure(invalidBirthdateMsg, Vector.empty)
    }
  }

  def validPhone(phoneNumber: String): Validation[String, String] = {
    if (phoneNumber.matches("[0-9]{10}"))
      Success(phoneNumber)
    else
      Failure(invalidPhoneNumberMsg, Vector.empty)
  }

  def validWebForm(name: String, birthdate: String, phone: String): Validation[String, WebForm] =
    Applicatives.validationApplicative[String].map3(
      validName(name),
      validBirthdate(birthdate),
      validPhone(phone)
    )(WebForm)
}