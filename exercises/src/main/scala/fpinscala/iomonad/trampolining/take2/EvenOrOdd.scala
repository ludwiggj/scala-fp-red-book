package fpinscala.iomonad.trampolining.take2

object EvenOrOdd {
  def even[A](lst: List[A]): Trampoline[Boolean] = {
    lst match {
      case Nil => Done(true)
      case _ :: xs => More(() => odd(xs))
    }
  }

  def odd[A](lst: List[A]): Trampoline[Boolean] = {
    lst match {
      case Nil => Done(false)
      case _ :: xs => More(() => even(xs))
    }
  }

  def main(args: Array[String]): Unit = {
    println(even((0 to 100000).toList).runT)
    println(even((0 to 100001).toList).runT)
  }
}
