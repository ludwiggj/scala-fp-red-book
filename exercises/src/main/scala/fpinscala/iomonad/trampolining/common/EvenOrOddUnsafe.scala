package fpinscala.iomonad.trampolining.common

object EvenOrOddUnsafe {
  def even[A](lst: List[A]): Boolean = {
    lst match {
      case Nil => true
      case _ :: xs => odd(xs)
    }
  }

  def odd[A](lst: List[A]): Boolean = {
    lst match {
      case Nil => false
      case _ :: xs => even(xs)
    }
  }

  def main(args: Array[String]): Unit = {
    even((0 to 1000000).toList) // blows the stack
  }

  // For stack safety, we need to change our functions to build a
  // description of a program instead of actually making any recursive
  // calls. Then, we would need some sort of generic function that
  // will go through our description and evaluate each recursion step.
}
