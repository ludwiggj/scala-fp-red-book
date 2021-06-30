package fpinscala.applicative

object OptionApplicativeVsMonad {
  val F: Applicative[Option] = new Applicative[Option] {
    override def unit[A](a: => A): Option[A] = Some(a)

    override def map2[A, B, C](fa: Option[A], fb: Option[B])(f: (A, B) => C): Option[C] =
      fa.flatMap(a => fb map (b => f(a, b)))
  }

  val departmentsByName: Map[String, String] = Map("Alice" -> "Engineering", "Mike" -> "Sales")

  val salariesByName: Map[String, Double] = Map("Alice" -> 50000, "Kira" -> 35000)

  val aliceDetails: Option[String] = F.map2(departmentsByName.get("Alice"), salariesByName.get("Alice")) {
    (dept, salary) => s"Alice in $dept makes $salary per year"
  }

  val idsByName: Map[String, Int] = Map("Alice" -> 1, "Mike" -> 2, "Kira" -> 3)

  val departmentsById: Map[Int, String] = Map(1 -> "Engineering", 2 -> "Sales")

  val salariesById: Map[Int, Double] = Map(1 -> 50000, 2 -> 23000, 3 -> 35000)

  val mikeDetails: Option[String] = idsByName.get("Mike").flatMap { id =>
    F.map2(departmentsById.get(id), salariesById.get(id)) {
      (dept, salary) => s"Mike in $dept makes $salary per year"
    }
  }

  def main(args: Array[String]): Unit = {
    println(aliceDetails)
    println(mikeDetails)
  }
}
