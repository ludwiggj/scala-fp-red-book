package fpinscala.iomonad.trampolining.runar.take4

import State.{getState, pureState, setState}

object StateWorkout {
  def zipIndex[A](as: List[A]): List[(Int, A)] =
    as.foldLeft(
      pureState[Int, List[(Int, A)]](List())
    )((acc, a) => for {
      xs <- acc
      n <- getState
      _ <- setState(n + 1)
    } yield (n, a) :: xs).runS(0).runT._1.reverse

  def main(args: Array[String]): Unit = {
    // State flatMap implementation turns out to be insufficient. zipIndex still
    // overflows the stack for large lists, this time for even smaller lists.

    // The problem is that the call to runT is not in the tail position, so it
    // can’t be optimized or wrapped in a Trampoline.
    println(zipIndex(('A' to 'Z').toList).last)

    // blows the stack above this (previous limit, take 1, was 3499)
    println(zipIndex((0 to 2499).toList).last)
  }
}