package fpinscala.iomonad.trampolining.take1

// An algebraic data type to represent our recursive program
sealed trait Trampoline[A]

// Done represents case where there are no computations to be done and we can yield a value
case class Done[A](value: A) extends Trampoline[A]

// More represents case where there is a recursive function call to be made
case class More[A](call: () => Trampoline[A]) extends Trampoline[A] {
  override def toString: String = s"More(call: () => ${call()})"
}

object Trampoline {
  // Example value
  val z = More(() => More(() => Done(42)))
}