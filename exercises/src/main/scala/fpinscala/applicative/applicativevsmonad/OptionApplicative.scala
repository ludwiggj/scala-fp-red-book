package fpinscala.applicative.applicativevsmonad

import fpinscala.applicative.Applicative

object OptionApplicative {
  val F: Applicative[Option] = new Applicative[Option] {
    override def unit[A](a: => A): Option[A] = Some(a)

    override def apply[A, B](fab: Option[A => B])(fa: Option[A]): Option[B] = {
      (fab, fa) match {
        case (Some(ab), Some(a)) => Some(ab(a))
        case _ => None
      }
    }
  }
}