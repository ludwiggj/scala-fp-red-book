package fpinscala.iomonad.trampolining.runar.take6

import scala.annotation.tailrec

trait Trampoline[+A] {
  final def resume : Either [() => Trampoline[A], A] = this match {
      case Done(v) =>
        println(s"Done($v)")
        Right(v)

      case More(k) =>
        println("More(() => Trampoline))")
        Left(k)

      case FlatMap(a, ka) => a match {

        // IMPORTANT NOTE: There is one more corner case to consider with this solution. It’s
        // now possible for resume to overflow the stack if the left-leaning tower of
        // FlatMaps is taller than the call stack. Then the call ka(v) will make the call
        // kb(x), which will make another inner call, etc.

        // We avoid this by disallowing the construction of deeply nested left-associated
        // binds in the first place

        case Done(v) =>
          println(s"FlatMap(Done($v), A => Trampoline)")
          ka(v).resume

        // To close the gap, we must also prevent the resume method from constructing such a
        // tower, by replacing calls to the FlatMap constructor with calls to the flatMap method

        // case More(k) => Left(() => FlatMap(k(), ka))
        case More(k) =>
          println(s"FlatMap(More(() => Trampoline)), A => Trampoline)")
          Left(() => k() flatMap ka)

        // case FlatMap(b, kb) =>
        //  (FlatMap(b, (x: Any) => FlatMap(kb(x), ka)): Trampoline[A]).resume

        case FlatMap(b, kb) =>
          println(s"FlatMap(FlatMap(Trampoline, A => Trampoline), A => Trampoline)")
          b.flatMap((x: Any) => kb(x) flatMap ka).resume
      }
    }

  @tailrec
  final def runT: A = resume match {
    case Right(a) => a
    case Left(k) => k().runT
  }

  // Rewrite flatMap to always construct right-associated binds

  // def flatMap[B](f: A => Trampoline[B]): Trampoline[B] =
  //  More[B](() => f(runT))

  def flatMap[B](f: A => Trampoline[B]): Trampoline[B] =
    this match {
      case FlatMap(a, g) =>
        FlatMap(a, (x: Any ) => g(x) flatMap f)
      case x => FlatMap (x, f)
    }
}

case class Done[+A](v: A) extends Trampoline[A]

case class More[+A](k: () => Trampoline[A]) extends Trampoline[A]

// We make the FlatMap constructor private
case class FlatMap[A, +B] private(
                           sub : Trampoline[A],
                           k: A => Trampoline[B]) extends Trampoline[B]
