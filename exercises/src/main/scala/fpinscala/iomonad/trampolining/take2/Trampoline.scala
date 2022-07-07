package fpinscala.iomonad.trampolining.take2

import scala.annotation.tailrec

// Consolidated methods under Trampoline
trait Trampoline[+A] {
  def resume: Either[() => Trampoline[A], A] = this match {
    case Done(v) => Right(v)
    case More(m) => Left(m)
  }

  @tailrec
  final def runT: A = resume match {
    case Right(value) => value
    case Left(more) => more().runT
  }
}

case class Done[A](value: A) extends Trampoline[A]

case class More[A](call: () => Trampoline[A]) extends Trampoline[A]