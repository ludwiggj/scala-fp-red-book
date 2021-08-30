package fpinscala.applicative

import fpinscala.monads.Functor

import scala.language.higherKinds

// The name applicative comes from the fact that we can formulate the applicative
// interface using the primitives unit and apply
trait Applicative[F[_]] extends Functor[F] {

  // primitive
  def unit[A](a: => A): F[A]

  // Defining apply allows us to write following:
  // new Applicative[T]

  // primitive
  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)(_(_))

  // derived combinator
  def map[A,B](fa: F[A])(f: A => B): F[B] = {
    apply(unit(f))(fa)
  }

  // derived combinator
  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    apply(map(fa)(f.curried))(fb)
  }

  // derived combinator
  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, flb) => map2(f(a), flb)(_ :: _))

  // derived combinator
  def sequence[A](fas: List[F[A]]): F[List[A]] =
    traverse(fas)(identity)

  // derived combinator
  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  // derived combinator
  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((_, _))

  // Exercise 12.3

  // The pattern is simple. We just curry the function we want to lift, pass the result to `unit`,
  // and then `apply` as many times as there are arguments.
  // Each call to `apply` is a partial application of the function
  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = {
    apply(apply(apply(unit(f.curried))(fa))(fb))(fc)
  }

  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] = {
    apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)
  }

  // Exercise 12.8
  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = {
    // NOTE: This gives a different result to super
    val self = this

    new Applicative[({type f[x] = (F[x], G[x])})#f] {
      override def unit[A](a: => A): (F[A], G[A]) = {
        (self.unit(a), G.unit(a))
      }

      override def map2[A, B, C](a: (F[A], G[A]), b: (F[B], G[B]))(f: (A, B) => C): (F[C], G[C]) = {
        val fab: F[(A, B)] = self.product(a._1, b._1)
        val gab: G[(A, B)] = G.product(a._2, b._2)

        val fc: F[C] = self.map(fab) { case (fa, fb) => f(fa, fb) }
        val gc: G[C] = G.map(gab) { case (ga, gb) => f(ga, gb) }

        (fc, gc)
      }

      override def apply[A, B](fab: (F[A => B], G[A => B]))(fa: (F[A], G[A])): (F[B], G[B]) = {
        (self.apply(fab._1)(fa._1), G.apply(fab._2)(fa._2))
      }
    }
  }

  // Exercise 12.9
  // Hint: The definition of `map2` is very short. The only things you can do are `map2` and `unit`
  //       from the `F` and `G` applicatives. Follow the types.
  def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = {
    val self = this

    new Applicative[({type f[x] = F[G[x]]})#f] {
      override def unit[A](a: => A): F[G[A]] =
        self.unit(G.unit(a))

      override def apply[A, B](fab: F[G[A => B]])(fa: F[G[A]]): F[G[B]] =
      // self.map2(fab, fa)((gab, ga) => G.map2(gab, ga)((ab, a) => ab(a)))
        self.map2(fab, fa)(G.map2(_, _)(_ (_)))

      override def map2[A, B, C](fga: F[G[A]], fgb: F[G[B]])(f: (A, B) => C): F[G[C]] =
        self.map2(fga, fgb)(G.map2(_, _)(f))
    }
  }

  // TODO - Exercise 12.10
  //        Prove that this composite applicative functor meets the applicative laws

  // Exercise 12.12
  // Hint: The standard library lets you treat a `Map` as essentially a list of pairs.
  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] = {
    map(traverse(ofa.toList) { case (k, fv) => map2(unit(k), fv)((_, _)) })(_.toMap)
  }

  def sequenceMapTextbook[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] =
    ofa.foldLeft(unit(Map.empty[K, V])) { case (acc, (k, fv)) =>
      map2(acc, fv)((m, v) => m + (k -> v))
    }
}

// For exercise 12.13. See https://blog.buildo.io/elegant-retrieval-of-type-class-instances-in-scala-32a524bbd0a7
object Applicative {
    def apply[F[_]](implicit e: Applicative[F]): Applicative[F] = e
}