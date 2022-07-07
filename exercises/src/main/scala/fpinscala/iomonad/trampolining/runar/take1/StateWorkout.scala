package fpinscala.iomonad.trampolining.runar.take1
import State.{getState, pureState, setState}

object StateWorkout {
  def zipIndex[A](as: List[A]): List[(Int, A)] =
    as.foldLeft(
      pureState[Int, List[(Int, A)]](List())
    )((acc, a) => for {
      xs <- acc
      n <- getState
      _ <- setState(n + 1)
    } yield (n, a) :: xs).runS(0)._1.reverse

  def main(args: Array[String]): Unit = {
    println(zipIndex(('A' to 'Z').toList).last)

    // blows the stack above this
    println(zipIndex((0 to 3499).toList).last)
  }
}