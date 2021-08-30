package fpinscala.applicative.applicativeVsMonadSpec

import fpinscala.UnitSpec
import fpinscala.applicative.applicativevsmonad.OptionApplicative.F

import scala.language.higherKinds

class OptionApplicativeSpec extends UnitSpec {

  "option applicative" can "run independent computations" in {
    val departmentsByName: Map[String, String] = Map("Alice" -> "Engineering", "Mike" -> "Sales")

    val salariesByName: Map[String, Double] = Map("Alice" -> 50000, "Kira" -> 35000)

    val aliceDetails: Option[String] = F.map2(departmentsByName.get("Alice"), salariesByName.get("Alice")) {
      (dept, salary) => s"Alice in $dept makes $salary per year"
    }
    assert(aliceDetails.contains("Alice in Engineering makes 50000.0 per year"))
  }

  "option monad" can "run dependent calculations" in {
    val idsByName: Map[String, Int] = Map("Alice" -> 1, "Mike" -> 2, "Kira" -> 3)

    val departmentsById: Map[Int, String] = Map(1 -> "Engineering", 2 -> "Sales")

    val salariesById: Map[Int, Double] = Map(1 -> 50000, 2 -> 23000, 3 -> 35000)

    val mikeDetails: Option[String] = idsByName.get("Mike").flatMap { id =>
      F.map2(departmentsById.get(id), salariesById.get(id)) {
        (dept, salary) => s"Mike in $dept makes $salary per year"
      }
    }

    assert(mikeDetails.contains("Mike in Sales makes 23000.0 per year"))
  }
}