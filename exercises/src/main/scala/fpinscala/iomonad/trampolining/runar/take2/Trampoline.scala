package fpinscala.iomonad.trampolining.runar.take2

import scala.annotation.tailrec

// Trade stack for heap
trait Trampoline[+A] {
  // The runT method is a simple tail-recursive method that execute
  // all the steps. It is made final so that Scala can eliminate
  // the tail call.
  @tailrec
  final def runT: A = this match {
    case Done(v) => v
    case More(k) => k().runT
  }
}

// A step of the form Done(v) has a value v to return and there
// are no more steps in that case.
case class Done[+A](v: A) extends Trampoline[A]

// A step of the form More(k) has more work to do, where k is a
// closure that does some work and returns the next step.
case class More[+A](k: () => Trampoline[A]) extends Trampoline[A]