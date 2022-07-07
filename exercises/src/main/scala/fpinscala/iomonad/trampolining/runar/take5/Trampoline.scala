package fpinscala.iomonad.trampolining.runar.take5

import scala.annotation.tailrec

// Trampoline monad
trait Trampoline[+A] {
  // The runT method must now take the FlatMap class into account. To simplify, let’s separate the
  // concern of advancing to the next step from the concern of running all the steps

  // This is the method that advances to the next step

  // The resume method proceeds by pattern matching on the Trampoline, returning either the result
  // (on the Right) or the next step as a Function0 (on the Left).
  final def resume : Either [() => Trampoline[A], A] = this match {
      case Done(v) =>
        println(s"Done($v)")
        Right(v)

      case More(k) =>
        println("More(() => Trampoline))")
        Left(k)

      // Pattern match on the subroutine call a
      case FlatMap(a, ka) => a match {
        // If it's Done we simply run the continuation

        // IMPORTANT NOTE: There is one more corner case to consider with this solution. It’s
        // now possible for resume to overflow the stack if the left-leaning tower of
        // FlatMaps is taller than the call stack. Then the call ka(v) will make the call
        // kb(x), which will make another inner call, etc.
        case Done(v) =>
          println(s"FlatMap(Done($v), A => Trampoline)")
          ka(v).resume

        // If it’s wrapped in a More constructor, we advance by one step and FlatMap over that.
        case More(k) =>
          println(s"FlatMap(More(() => Trampoline)), A => Trampoline)")
          Left(() => FlatMap(k(), ka))

        // Two flatmaps i.e. FlatMap(FlatMap(b, kb), ka)

        // The subroutine call itself contains a subroutine call, we have a left-associated
        // nesting of FlatMaps in an expression like this. The trick is to reassociate the
        // expression to the right

        // We must cast explicitly to Trampoline for the compiler to be able to figure out
        // that this is in fact a tail-recursive self-call

        // There is some type information that has been lost. In a pattern like
        // FlatMap(FlatMap(b, kb), ka) the type of b cannot be known, so we must assume Any
        // when we construct the right-associated nesting. This is perfectly safe, since we
        // can assume the left-associated nesting was well typed when it was constructed
        case FlatMap(b, kb) =>
          println(s"FlatMap(FlatMap(Trampoline, A => Trampoline), A => Trampoline)")
          (FlatMap(b, (x: Any) => FlatMap(kb(x), ka)): Trampoline[A]).resume
      }
    }

  @tailrec
  final def runT: A = resume match {
    case Right(a) => a
    case Left(k) => k().runT
  }

  // TODO - adjust?
  def flatMap[B](f: A => Trampoline[B]): Trampoline[B] =
    More[B](() => f(runT))
}

// Monadic unit
case class Done[+A](v: A) extends Trampoline[A]

case class More[+A](k: () => Trampoline[A]) extends Trampoline[A]

// The way around this limitation is to add a constructor to the Trampoline
// data type, changing flatMap from a method call to a constructor call.

// A trampoline of this form can be thought of as a call to a subroutine
// sub whose result is returned to the continuation k.
case class FlatMap[A, +B](
                           sub : Trampoline[A],
                           k: A => Trampoline[B]) extends Trampoline[B]
