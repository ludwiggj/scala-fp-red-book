package fpinscala.testing

object Gen {
  def unit[A](a: => A): Gen[A] = ???
}

trait Gen[A] {
  def map[B](f: A => B): Gen[B] = ???

  def flatMap[B](f: A => Gen[B]): Gen[B] = ???
}

trait SGen[+A] {
}