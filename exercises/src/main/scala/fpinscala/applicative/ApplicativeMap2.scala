package fpinscala
package applicative

import monads.Functor

// TODO - fix
//import StateUtil._ // defined at bottom of this file
import monoids._
import language.higherKinds
import language.implicitConversions

//noinspection NotImplementedCode

// All applicatives are functors
trait ApplicativeMap2[F[_]] extends Functor[F] {
  // primitive
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]

  // primitive
  def unit[A](a: => A): F[A]

  // derived combinator
  def map[A, B](fa: F[A])(f: A => B): F[B] =
    map2(fa, unit(()))((a, _) => f(a))

  // derived combinator
  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, flb) => map2(f(a), flb)(_ :: _))

  // Exercise 12.1 - transplant as many combinators as possible from Monad to
  //                 Applicative, using only unit and map2, and methods
  //                 implemented in terms of them (i.e. map and traverse)

  // Exercise 12.1 - sequence
  def sequence[A](fas: List[F[A]]): F[List[A]] =
    traverse(fas)(identity)

  // Exercise 12.1 - replicateM
  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  // Exercise 12.1 - product
  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((_, _)) // don't need to state identity here

  // Defining apply allows us to write the following:
  // new ApplicativeMap2[T]
  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)(_ (_))
}