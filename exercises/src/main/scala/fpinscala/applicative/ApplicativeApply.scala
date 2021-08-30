package fpinscala.applicative

import fpinscala.monads.Functor

import scala.language.higherKinds

// The name applicative comes from the fact that we can formulate the applicative
// interface using an alternative set of primitives (unit and the function apply)
// rather than unit and map2
trait ApplicativeApply[F[_]] extends Functor[F] {

  // primitive
  def unit[A](a: => A): F[A]

  // primitive
  // Exercise 12.2; implement apply in terms of map2 and unit
  def _apply[A,B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)((a2b, a) => a2b(a))

  // We simply use `map2` to lift a function into `F` so we can apply it
  // to both `fab` and `fa`. The function being lifted here is `_(_)`,
  // which is the same as the lambda notation `(f, x) => f(x)`. That is,
  // It's a function that takes two arguments:
  //   1. A function `f`
  //   2. An argument `x` to that function
  // and it simply applies `f` to `x`.

  // Exercise 12.2; implement apply in terms of map2 and unit (textbook version)
  def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)(_(_))

  // Exercise 12.2; implement map2 in terms of apply and unit
  def _map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    val fabc = unit[A => B => C]((a: A) => f.curried(a))
    val fbc = apply(fabc)(fa)
    apply(fbc)(fb)
  }

  // `map2` is implemented by first currying `f` so we get a function
  // of type `A => B => C`. This is a function that takes `A` and returns
  // another function of type `B => C`. So if we map `f.curried` over an
  // `F[A]`, we get `F[B => C]`. Passing that to `apply` along with the
  // `F[B]` will give us the desired `F[C]`.

  // Exercise 12.2; implement map2 in terms of apply and unit (textbook version)
  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    apply(map(fa)(f.curried))(fb)
  }

  // Exercise 12.2; implement map in terms of apply and unit
  def _map[A,B](fa: F[A])(f: A => B): F[B] = {
    val fab = unit[A => B]((a: A) => f(a))
    apply(fab)(fa)
  }

  // Exercise 12.2; implement map in terms of apply and unit (textbook version)
  def map[A,B](fa: F[A])(f: A => B): F[B] = {
    apply(unit(f))(fa)
  }
}