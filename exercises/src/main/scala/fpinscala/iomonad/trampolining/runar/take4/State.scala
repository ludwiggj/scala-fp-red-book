package fpinscala.iomonad.trampolining.runar.take4

case class State[S, +A](runS: S => Trampoline[(A, S)]) {
  def map[B](f: A => B): State[S, B] = State[S, B](
    runS.andThen { tramp => {
      val (a, s) = tramp.runT
      Done(f(a), s)
    }}
  )

  // By adding a flatMap method directly to Trampoline, we can rewrite State.flatMap
  // It’s a definite improvement; it shifts the problem into the flatMap method for
  // Trampoline
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State[S, B](s =>
      More(
        () => runS(s) flatMap {
          case (a, s1) => More (() => f(a) runS s1)
        }
      )
    )
}

object State {
  def pureState[S, A](a: A): State[S, A] =
    State(s => More(() => Done(a, s)))

  // Gets the current state (passed in as input) and maintains state, and returns it as result
  def getState[S]: State[S, S] = State(s => More(() => Done((s, s))))

  // Sets the new state to `s`, throws away result
  def setState[S](s: S): State[S, Unit] = State(_ => More(() => Done((), s)))
}