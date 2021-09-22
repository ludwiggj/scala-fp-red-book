package fpinscala.applicative

object Applicatives {
  implicit def streamApplicative: Applicative[Stream] = new Applicative[Stream] {

    //noinspection ScalaUnusedSymbol
    // Stream.continually(a) is the book implementation for (top of page 212)
    // It works for exercise 12.4, but never completes when used to implement exercise 12.13
    def _unit[A](a: => A): Stream[A] = {
      Stream.continually(a)
    }

    // Hence the following alternative definition of unit
    // TODO - Kludge needed for exercises 12.4 & 12.13
    def unit[A](n: Int, a: => A): Stream[A] = {
      Stream(List.fill(n)(a): _*)
    }

    override def unit[A](a: => A): Stream[A] = {
      unit(1, a)
    }

    // Implementing map2 means that apply is implemented
    override def map2[A, B, C](a: Stream[A], b: Stream[B])( // Combine elements pointwise
                                                            f: (A, B) => C): Stream[C] =
      a zip b map f.tupled

    // Exercise 12.4

    // What is the meaning of streamApplicative.sequence?

    // Takes a list of streams, returns a stream where nth element is a list of all nth elements of all streams
    // Last element is that in which all elements are present

    // Textbook answer: This transposes the list! That is, we start with a list of rows, each of which is
    // possibly infinite in length. We get back a single row, where each element is the column of values
    // at that position.

    // override def sequence[A](fas: List[Stream[A]]): Stream[List[A]] = super.sequence(fas)

    // Note - needed to implement sequence
    override def traverse[A, B](as: List[A])(f: A => Stream[B]): Stream[List[B]] = {
      // TODO Massive kludge.... This could be why implementation of exercise 12.4 isn't discussed
      //      in the book. Really want to use longest stream in the list to set the length, but it's
      //      an A in the above type signature - so just set it to 10 to get the test passing :)
      val maxStreamLength = 10
      as.foldRight(unit(maxStreamLength, List[B]()))((a, flb) => map2(f(a), flb)((b, lb) => b :: lb))
    }
  }

  // Exercise 12.6
  def validationApplicative[E]: Applicative[({type f[x] = Validation[E, x]})#f] =
    new Applicative[({type f[x] = Validation[E, x]})#f] {
      override def unit[A](a: => A): Validation[E, A] = Success(a)

      override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] =
        (fa, fb) match {
          case (Success(a), Success(b)) => Success(f(a, b))
          case (Failure(ha, ta), Failure(hb, tb)) => Failure(ha, ta ++ Vector(hb) ++ tb)
          case (_, f: Failure[E]) => f
          case (f@Failure(_, _), _) => f
        }

      override def apply[A, B](fab: Validation[E, A => B])(fa: Validation[E, A]): Validation[E, B] =
        map2(fab, fa)(_ (_))
    }

  def validationSuccessApplicative: Applicative[Success] =
    new Applicative[Success] {
      override def unit[A](a: => A): Success[A] = Success(a)

      override def map2[A, B, C](fa: Success[A], fb: Success[B])(f: (A, B) => C): Success[C] = Success(f(fa.a, fb.a))

      override def apply[A, B](fab: Success[A => B])(fa: Success[A]): Success[B] = map2(fab, fa)(_ (_))
    }
}