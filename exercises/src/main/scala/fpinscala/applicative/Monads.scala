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

  // Exercise 12.20
  def composeM[F[_], N[_]](implicit F: Monad[F], N: Monad[N], T: Traverse[N]): Monad[({type f[x] = F[N[x]]})#f] =
    new Monad[({type f[x] = F[N[x]]})#f] {
      override def unit[A](a: => A): F[N[A]] = F.unit(N.unit(a))

      override def map[A, B](fa: F[N[A]])(f: A => B): F[N[B]] = F.map(fa)(na => N.map(na)(f))

      override def join[A](mma: F[N[F[N[A]]]]): F[N[A]] = {
        F.join(F.map(mma)(nfna => F.map(T.sequence(nfna))(N.join)))
      }

      override def flatMap[A, B](ma: F[N[A]])(f: A => F[N[B]]): F[N[B]] = {
//        val fnfnb = F.map(ma)(na => N.map(na)(a => f(a)))
//        val fnnb = F.join(F.map(fnfnb)(nfnb => T.sequence(nfnb)))
//        F.map(fnnb)(N.join)
        F.map(F.join(F.map(F.map(ma)(na => N.map(na)(a => f(a))))(nfnb => T.sequence(nfnb))))(N.join)
      }

      def flatMapTextbook[A,B](fna: F[N[A]])(f: A => F[N[B]]): F[N[B]] = {
        // Type of flatMap statement is:
        // flatMap[N[A], N[B]](fna: F[N[A]])(f: N[A] => F[N[B]]): F[N[B]]

        // Type of traverse statement is:
        // N.traverse[F, A, N[B]](na: N[A])(f: A => F[N[B]): F[N[N[B]]]
        F.flatMap[N[A], N[B]](fna)(na => F.map(T.traverse[F, A, N[B]](na)(f))(N.join))
      }
    }

  // Example of monad transformer
  // OptionT composes Option with any other monad
  case class OptionT[M[_], A](value: M[Option[A]])(implicit M: Monad[M]) {
    def flatMap[B](f: A => OptionT[M, B]): OptionT[M, B] = {
      // Part inside brackets OptionT(...) needs to evaluate to M[Option[B]]
      // def flatMap[Option[A], Option[B]](ma: M[Option[A]])(f: Option[A] => M[Option[B]]): M[Option[B]]
      OptionT[M, B](M.flatMap[Option[A], Option[B]](value) {
        case None => M.unit(None)
        case Some(a) => f(a).value
      })
    }
  }
}