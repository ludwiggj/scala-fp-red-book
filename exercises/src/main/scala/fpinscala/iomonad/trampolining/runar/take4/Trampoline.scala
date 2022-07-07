package fpinscala.iomonad.trampolining.runar.take4

import scala.annotation.tailrec

// Trampoline monad
trait Trampoline[+A] {
  @tailrec
  final def runT: A = this match {
    case Done(v) => v
    case More(k) => k().runT
  }

  // We will attempt to solve problem of State flatmap function by making
  // Trampoline monadic. It already has a monadic unit which is the Done
  // constructor. All it needs is monadic bind, which is flatMap.

  // But this is not what we want. The call to runT is not in a tail
  // position. It seems impossible to implement a flatMap method for
  // Trampoline that doesn’t require any additional stack.
  def flatMap[B](f: A => Trampoline[B]): Trampoline[B] =
    More[B](() => f(runT))
}

// Monadic unit
case class Done[+A](v: A) extends Trampoline[A]

case class More[+A](k: () => Trampoline[A]) extends Trampoline[A]