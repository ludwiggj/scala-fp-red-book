package fpinscala.iomonad

import language.higherKinds // Disable warnings for type constructor polymorphism
import language.implicitConversions

trait Functor[F[_]] {
  def map[A,B](a: F[A])(f: A => B): F[B]
}

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]

  def flatMap[A,B](a: F[A])(f: A => F[B]): F[B]

  def map[A,B](a: F[A])(f: A => B): F[B] = flatMap(a)(a => unit(f(a)))

  def map2[A,B,C](a: F[A], b: F[B])(f: (A,B) => C): F[C] =
    flatMap(a)(a => map(b)(b => f(a,b)))

  // Sequences F instances, flatMapping them together
  def sequence_[A](fs: Stream[F[A]]): F[Unit] = foreachM(fs)(skip)

  // Sequences F instances, flatMapping them together
  def sequence_[A](fs: F[A]*): F[Unit] = sequence_(fs.toStream)

  // Replicate F instances, fold into list
  def replicateM[A](n: Int)(f: F[A]): F[List[A]] =
    Stream.fill(n)(f).foldRight(unit(List[A]()))(map2(_,_)(_ :: _))

  // Replicate F instances, fold into list, discarding result
  def replicateM_[A](n: Int)(f: F[A]): F[Unit] =
    foreachM(Stream.fill(n)(f))(skip)

  // Replace result
  def as[A,B](a: F[A])(b: B): F[B] = map(a)(_ => b)

  // Discard result i.e. replace with unit
  def skip[A](a: F[A]): F[Unit] = as(a)(())

  // map the effect to true when condition is true, otherwise
  // discard the effect and return false
  def when[A](b: Boolean)(fa: => F[A]): F[Boolean] =
    if (b) as(fa)(true) else unit(false)

  // Repeats the effect of its argument infinitely
  def forever[A,B](a: F[A]): F[B] = {
    lazy val t: F[B] = a flatMap (_ => t)
    t
  }

  // Repeat the effect while the condition is true
  // TODO: The effect is discarded, not flatmapped together
  def while_(a: F[Boolean])(b: F[Unit]): F[Unit] = {
    lazy val t: F[Unit] = while_(a)(b)
    a flatMap (c => skip(when(c)(t)))
  }

  // Repeats the effect of the first argument as
  // long as the cond function yields true
  def doWhile[A](a: F[A])(cond: A => F[Boolean]): F[Unit] = for {
    a1 <- a
    ok <- cond(a1)
    _ <- if (ok) doWhile(a)(cond) else unit(())
  } yield ()

  // Folds the stream with the function f, combining
  // the effects and returning the result
  def foldM[A,B](l: Stream[A])(z: B)(f: (B,A) => F[B]): F[B] =
    l match {
      case h #:: t => f(z,h) flatMap (z2 => foldM(t)(z2)(f))
      case _ => unit(z)
    }

  // The same as the foldM function above except ignores the result
  def foldM_[A,B](l: Stream[A])(z: B)(f: (B,A) => F[B]): F[Unit] =
    skip { foldM(l)(z)(f) }

  // Calls the function f for each element of the stream and
  // combines the effects
  def foreachM[A](l: Stream[A])(f: A => F[Unit]): F[Unit] =
    foldM_(l)(())((_, a) => skip(f(a)))


  def seq[A,B,C](f: A => F[B])(g: B => F[C]): A => F[C] =
    f andThen (fb => flatMap(fb)(g))

  // syntax
  implicit def toMonadic[A](a: F[A]): Monadic[F,A] =
    new Monadic[F,A] { val F = Monad.this; def get: F[A] = a }
}

trait Monadic[F[_],A] {
  val F: Monad[F]
  def get: F[A]
  private val a = get
  def map[B](f: A => B): F[B] = F.map(a)(f)
  def flatMap[B](f: A => F[B]): F[B] = F.flatMap(a)(f)
  def **[B](b: F[B]): F[(A, B)] = F.map2(a,b)((_,_))
  def *>[B](b: F[B]): F[B] = F.map2(a,b)((_,b) => b)
  def map2[B,C](b: F[B])(f: (A,B) => C): F[C] = F.map2(a,b)(f)
  def as[B](b: B): F[B] = F.as(a)(b)
  def skip: F[Unit] = F.skip(a)
  def replicateM(n: Int): F[List[A]] = F.replicateM(n)(a)
  def replicateM_(n: Int): F[Unit ] = F.replicateM_(n)(a)
}

