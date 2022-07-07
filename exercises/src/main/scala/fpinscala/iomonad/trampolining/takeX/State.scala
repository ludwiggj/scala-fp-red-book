package fpinscala.iomonad.trampolining.takeX

// case class State[S, +A](run: S => (A, S)) {
//
//   def map[B](f: A => B): State[S, B] = State(
//     s => {
//       val (a, s1) = run(s)
//       (f(a), s1)
//     }
//   )
//
//   def flatMap[B](f: A => State[S, B]): State[S, B] = State(
//     s => {
//       val (a, s1) = run(s)
//       f(a).run(s1)
//     }
//   )
// }

case class State[S, +A](runS: S => Trampoline[(S, A)]) {

  def map[B](f: A => B): State[S, B] = State[S, B](
      runS.andThen { tramp => {
        val (s, a) = tramp.runT
        Done(s, f(a))
      }
    }
  )

  def flatMap[B](f: A => State[S, B]): State[S, B] = State[S, B](
    runS.andThen { tramp => {
      FlatMap[(S, A), (S, B)](tramp, { case (s, a) => f(a).runS(s) })
    }}
  )
}

object State {
  def unit[S, A](a: A): State[S, A] =
    State(s => More(() => Done(s, a)))

  // Gets the current state (passed in as input) and maintains state, and returns it as result
  def get[S]: State[S, S] = State(s => More(() => Done((s, s))))

  // Sets the new state to `s`, throws away result
  def set[S](s: S): State[S, Unit] = State(_ => More(() => Done(s, ())))
}

object StateWorkout {
  def zipIndex[A](as: Seq[A]): State[Int, List[(Int, A)]] =
    as.foldLeft(State.unit[Int, List[(Int, A)]](List()))((acc, a) => for {
      xs <- acc
      n <- State.get
      _ <- State.set(n + 1)
    } yield (n, a) :: xs)

  def main(args: Array[String]): Unit = {
    println(zipIndex('A' to 'Z').runS(0).runT)
    println(zipIndex(0 to 2000).runS(0).runT._1) // blows the stack above this
  }
}