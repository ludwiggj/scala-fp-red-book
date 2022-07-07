package fpinscala.iomonad.trampolining.take3

// This uses previous definition of state
import fpinscala.state.State

object StateWorkout {
  def zipIndex[A](as: Seq[A]): State[Int, List[(Int, A)]] =
    as.foldLeft(State.unit[Int, List[(Int, A)]](List()))((acc, a) => for {
      xs <- acc
      n <- State.get
      _ <- State.set(n + 1)
    } yield (n, a) :: xs)

  def main(args: Array[String]): Unit = {
    println(zipIndex('A' to 'Z').run(0))

    // blows the stack above this
    println(zipIndex(0 to 3499).run(0)._2)
  }
}