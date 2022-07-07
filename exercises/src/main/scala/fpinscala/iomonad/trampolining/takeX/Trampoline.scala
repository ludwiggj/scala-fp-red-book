package fpinscala.iomonad.trampolining.takeX

import scala.annotation.tailrec

sealed trait Trampoline[+A] {
  def resume: Either[() => Trampoline[A], A] = this match {
    case Done(v) => Right(v)
    case More(m) => Left(m)
    case FlatMap(sub, cont) => sub match {
      case Done(v) => cont(v).resume
      case More(m) => Left(() => FlatMap(m(), cont))
      case FlatMap(sub2, cont2) =>
        (FlatMap(sub2, (x: Any) => FlatMap(cont2(x), cont)):Trampoline[A]).resume
    }
  }

  @tailrec
  final def runT: A = resume match {
    case Right(value) => value
    case Left(more) => more().runT
  }
}

case class Done[A](value: A) extends Trampoline[A]

case class More[A](call: () => Trampoline[A]) extends Trampoline[A]

case class FlatMap[A, B](sub: Trampoline[A], cont: A => Trampoline[B]) extends Trampoline[B]