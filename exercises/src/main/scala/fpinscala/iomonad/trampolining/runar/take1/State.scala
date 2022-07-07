package fpinscala.iomonad.trampolining.runar.take1

case class State[S, +A](runS: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    State[S, B](
      s => {
        val (a, s1) = runS(s)
        (f(a), s1)
      }
    )

  def flatMap[B](f:A => State [S, B]): State[S, B] =
    State[S, B](
      s => {
        val (a, s1) = runS(s)
        f(a) runS s1
      }
    )
}

object State {
  def getState[S]: State[S, S] =
    State(s => (s,s))

  def setState[S](s: S): State [S, Unit] =
    State(_ => ((), s))

  def pureState[S, A](a: A): State[S, A] =
    State (s => (a,s))
}