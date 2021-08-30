package fpinscala.applicative

import scala.language.higherKinds

// We can make Monad[F] a subtype of Applicative[F] by providing the default implementation of
// map2 in terms of flatMap
// i.e. all monads are applicative functors

// A minimal implementation of Monad must implement:
//
// (1) unit and flatMap
// OR
// (2) unit and compose
// OR
// (3) unit, map and join

// A minimal implementation of Applicative must implement:
//
// (1) unit and map2
// OR
// (2) unit and apply
trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B] = join(map(ma)(f))

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(identity)

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  override def apply[A, B](mf: F[A => B])(ma: F[A]): F[B] =
    flatMap(mf)(fab => {
      val fb = map(ma)(a => fab(a))
      fb
    })

  // A default implementation of map2 in terms of flatMap and map
  // Map is implemented in Applicative in terms of apply and unit
  override def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))

  // Alternatively map can be defined in Monad as follows:
  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)((a: A) => unit(f(a)))

  // TODO - Exercise 12.7
  //        Prove that all monads are applicative functors by showing that
  //        if the monad laws hold, the monad implementations of map2 and
  //        map satisfy the applicative laws

  // TODO - Exercise 12.11
  //        Try to write compose on Monad. It's not possible, but it is
  //        instructive to attempt it and understand why this is the case.
}