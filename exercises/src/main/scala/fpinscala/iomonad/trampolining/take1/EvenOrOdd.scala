package fpinscala.iomonad.trampolining.take1

import scala.annotation.tailrec

object EvenOrOdd {
  // Updated version where functions create data structure rather than recurse (or call other functions)
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

  @tailrec
  def run[A](trampoline: Trampoline[A]): A = {
    trampoline match {
      case Done(v) => v
      case More(t) => run(t()) // <- tail recursive, yay
    }
  }

  def main(args: Array[String]): Unit = {
    println(even((0 to 5).toList))
    println(run(even((0 to 100000).toList)))
    println(run(even((0 to 100001).toList)))
  }
}
