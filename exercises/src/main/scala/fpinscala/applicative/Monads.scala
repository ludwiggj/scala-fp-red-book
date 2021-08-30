package fpinscala.applicative

import fpinscala.state.State

import scala.language.higherKinds

//noinspection NotImplementedCode,ScalaUnusedSymbol
object Monads {
  // Exercise 12.5
  trait MonadWithError[F[_]] extends Monad[F] {
    // Kludge for testing
    def error[A](e: => F[A]): F[A]
  }

  def eitherMonad[E]: MonadWithError[({type f[x] = Either[E, x]})#f] = new MonadWithError[({type f[x] = Either[E, x]})#f] {
    // def unit[A](a: => A): F[A]
    // def unit[A](a: => A): Either[E, A]
    override def unit[A](a: => A): Either[E, A] = Right(a)

    // def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]
    // def flatMap[A, B](ma: Either[E, A])(f: A => Either[E, B]): Either[E, B]
    override def flatMap[A, B](ma: Either[E, A])(f: A => Either[E, B]): Either[E, B] = ma match {
      case Left(e) => Left(e)
      case Right(a) => f(a)
    }

    // Textbook implementation
    def _flatMap[A, B](ma: Either[E, A])(f: A => Either[E, B]): Either[E, B] =
      ma.flatMap(f)

    override def error[A](e: => Either[E, A]): Either[E, A] = e
  }

  def stateMonad[S]: Monad[({type f[x] = State[S, x]})#f] = new Monad[({type f[x] = State[S, x]})#f] {
    // def unit[A](a: => A): F[A]
    // def unit[A](a: => A): State[S, A]
    override def unit[A](a: => A): State[S, A] = State(s => (a, s))

    // def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]
    // def flatMap[A, B](ma: State[S, A])(f: A => State[S, B]): State[S, B]
    override def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }

  def composeM[F[_], N[_]](implicit F: Monad[F], N: Monad[N], T: Traverse[N]):
  Monad[({type f[x] = F[N[x]]})#f] = ???
}