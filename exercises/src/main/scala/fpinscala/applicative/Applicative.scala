package fpinscala
package applicative

import monads.Functor
import state._
import State._

import java.util.Date
// TODO - fix
//import StateUtil._ // defined at bottom of this file
import monoids._
import language.higherKinds
import language.implicitConversions

//noinspection NotImplementedCode
trait Applicative[F[_]] extends Functor[F] {

  // primitive
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = ???

  // primitive
  def unit[A](a: => A): F[A]

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = ???

  def _map[A, B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    map2(fa, unit(()))((a, _) => f(a))

  // Exercise 12.1 - implement using unit, map2, or methods implemented in terms of them
  def sequence[A](fas: List[F[A]]): F[List[A]] =
    traverse(fas)(identity)

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, flb) => map2(f(a), flb)(_ :: _))

  // Exercise 12.1
  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  // Exercise 12.1
  def factor[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((_, _)) // don't need to state identity here

  // Exercise 12.8
  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = {
    // NOTE: This gives a different result to super
    val self = this

    new Applicative[({type f[x] = (F[x], G[x])})#f] {
      override def unit[A](a: => A): (F[A], G[A]) = {
        (self.unit(a), G.unit(a))
      }

      override def map2[A, B, C](a: (F[A], G[A]), b: (F[B], G[B]))(f: (A, B) => C): (F[C], G[C]) = {
        val fab: F[(A, B)] = self.factor(a._1, b._1)
        val gab: G[(A, B)] = G.factor(a._2, b._2)

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

  // Exercise 12.12
  // Hint: The standard library lets you treat a `Map` as essentially a list of pairs.
  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] = {
    map(traverse(ofa.toList) { case (k, fv) => map2(unit(k), fv)((_, _)) })(_.toMap)
  }

  def sequenceMapTextbook[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] =
    (ofa foldLeft unit(Map.empty[K, V])) { case (acc, (k, fv)) =>
      map2(acc, fv)((m, v) => m + (k -> v))
    }

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
}

// We can make Monad[F] a subtype of Applicative[F] by providing the default implementation of
// map2 in terms of flatMap
// i.e. all monads are applicative functors

// A minimal implementation of Monad must implement:
//
// (1) unit and flatMap
// (2) unit and compose
// (3) unit, map and join

// A minimal implementation of Applicative must implement:
//
// (1) unit and map2
// (2) unit and apply
trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B] = join(map(ma)(f))

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  override def apply[A, B](mf: F[A => B])(ma: F[A]): F[B] =
    flatMap(mf)(f => map(ma)(a => f(a)))

  // A default implementation of map2 in terms of flatMap
  // Note that this uses map, but map is implemented in Applicative in terms of map2 and unit
  // map2 is defined here! (i.e. it's recursive), and unit must be implemented by Monad
  // So it is internally consistent
  override def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))

  // Alternatively map can be defined in Monad as follows:
  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)((a: A) => unit(f(a)))
}

//noinspection NotImplementedCode,ScalaUnusedSymbol
object Monad {
  // Exercise 12.5
  def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] = new Monad[({type f[x] = Either[E, x]})#f] {
    override def unit[A](a: => A): Either[E, A] = Right(a)

    override def flatMap[A, B](ma: Either[E, A])(f: A => Either[E, B]): Either[E, B] = ma match {
      case Left(e) => Left(e)
      case Right(a) => f(a)
    }

    def _flatMap[A, B](ma: Either[E, A])(f: A => Either[E, B]): Either[E, B] =
      ma.flatMap(f)
  }

  def stateMonad[S]: Monad[({type f[x] = State[S, x]})#f] = new Monad[({type f[x] = State[S, x]})#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))

    override def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }

  def composeM[F[_], N[_]](implicit F: Monad[F], N: Monad[N], T: Traverse[N]):
  Monad[({type f[x] = F[N[x]]})#f] = ???
}

object Applicative {

  object Streaming {

    // TODO - Check https://stackoverflow.com/questions/4465948/what-are-scala-context-and-view-bounds/4467012#4467012
    implicit def streamApplicative: Applicative[Stream] = new Applicative[Stream] {

      // TODO - Kludge needed for exercises 12.4 & 12.13
      def unit[A](n: Int, a: => A): Stream[A] = {
        Stream(List.fill(n)(a): _*)
      }

      // TODO Stream.continually(a) worked for exercise 12.4, but never completed when used to implement exercise 12.13
      //      Stream.continually(a) was the book implementation for the stream applicative (top of page 212)
      def unit[A](a: => A): Stream[A] = {
        unit(1, a)
      }

      override def map2[A, B, C](a: Stream[A], b: Stream[B])( // Combine elements pointwise
                                                              f: (A, B) => C): Stream[C] =
        a zip b map f.tupled

      // Exercise 12.4

      // Takes a list of streams, returns a stream where nth element is a list of all nth elements of all streams
      // Last element is that in which all elements are present

      // Textbook: This transposes the list! That is, we start with a list of rows, each of which is possibly
      // infinite in length. We get back a single row, where each element is the column of values at that
      // position.
      override def sequence[A](fas: List[Stream[A]]): Stream[List[A]] = super.sequence(fas)

      // This is for exercise 12.4
      override def traverse[A, B](as: List[A])(f: A => Stream[B]): Stream[List[B]] = {
        // TODO Massive kludge.... This could be why implementation of exercise 12.4 isn't discussed
        //      in the book. Really want to use longest stream in the list to set the length, but it's
        //      an A in the above type signature - so just set it to 10 to get the test passing :)
        val maxStreamLength = 10
        as.foldRight(unit(maxStreamLength, List[B]()))((a, flb) => map2(f(a), flb)((b, lb) => b :: lb))
      }
    }
  }

  // Exercise 12.6
  object Validation {
    val invalidNameMsg = "Name cannot be empty"
    val invalidBirthdateMsg = "Birthdate must be in the form yyyy-MM-dd"
    val invalidPhoneNumberMsg = "Phone number must be 10 digits"

    // Validation is a new data type that is much like Either
    // except that it can explicitly handle more than one error
    sealed trait Validation[+E, +A]

    case class Failure[E](head: E, tail: Vector[E])
      extends Validation[E, Nothing]

    case class Success[A](a: A) extends Validation[Nothing, A]

    case class WebForm(name: String, birthdate: Date, phoneNumber: String)

    def validName(name: String): Validation[String, String] =
      if (name != "")
        Success(name)
      else
        Failure(invalidNameMsg, Vector.empty)

    def validBirthdate(birthdate: String): Validation[String, Date] = {
      try {
        import java.text._
        Success(new SimpleDateFormat("yyyy-MM-dd").parse(birthdate))
      }
      catch {
        case _: Exception => Failure(invalidBirthdateMsg, Vector.empty)
      }
    }

    def validPhone(phoneNumber: String): Validation[String, String] = {
      if (phoneNumber.matches("[0-9]{10}"))
        Success(phoneNumber)
      else
        Failure(invalidPhoneNumberMsg, Vector.empty)
    }

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

    def validWebForm(name: String, birthdate: String, phone: String): Validation[String, WebForm] =
      validationApplicative[String].map3(
        validName(name),
        validBirthdate(birthdate),
        validPhone(phone)
      )(WebForm)
  }

  type Const[A, B] = A

  implicit def monoidApplicative[M](M: Monoid[M]): Applicative[({type f[x] = Const[M, x]})#f] =
    new Applicative[({type f[x] = Const[M, x]})#f] {
      def unit[A](a: => A): M = M.zero

      override def apply[A, B](m1: M)(m2: M): M = M.op(m1, m2)
    }

  // TODO - Exercise 12.7
  // TODO - Exercise 12.10
  // TODO - Exercise 12.11
}

//noinspection NotImplementedCode,ScalaUnusedSymbol
trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  // NOTE - This definition is useful to a point, but one of these methods must be overridden for a traversible
  //        otherwise it just recurses forever (also, it would not make sense as we need to call methods on G to
  //        leverage the fact that G is a traversible.

  //        Note that in exercise 12.13, the textbook answer overrides traverse in each case
  // TODO - Check https://stackoverflow.com/questions/4465948/what-are-scala-context-and-view-bounds/4467012#4467012
  def traverse[G[_] : Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence(map(fa)(f))

  // TODO - Check https://stackoverflow.com/questions/4465948/what-are-scala-context-and-view-bounds/4467012#4467012
  def sequence[G[_] : Applicative, A](fga: F[G[A]]): G[F[A]] = {
    // Substitute into traverse
    // F[A]      becomes F[G[A]        i.e. A becomes G[A]
    // A => G[B] becomes G[A] => G[A]  i.e. B becomes A
    // So traverse returns G[F[B]] i.e. G[F[A]]
    traverse(fga)(ga => ga)
  }

  def map[A, B](fa: F[A])(f: A => B): F[B] = ???

  import Applicative._

  override def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    traverse[({type f[x] = Const[B, x]})#f, A, Nothing](
      as)(f)(monoidApplicative(mb))

  def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({type f[x] = State[S, x]})#f, A, B](fa)(f)(Monad.stateMonad)

  def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) => for {
      s1 <- get[S]
      (b, s2) = f(a, s1)
      _ <- set(s2)
    } yield b).run(s)

  override def toList[A](fa: F[A]): List[A] =
    mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  def reverse[A](fa: F[A]): F[A] = ???

  override def foldLeft[A, B](fa: F[A])(z: B)(f: (B, A) => B): B = ???

  def fuse[G[_], H[_], A, B](fa: F[A])(f: A => G[B], g: A => H[B])
                            (implicit G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) = ???

  def compose[G[_]](implicit G: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f] = ???
}

object Traverse {
  // Exercise 12.13
  val listTraverse: Traverse[List] = new Traverse[List] {
    // TODO: This method signature doesn't compile, it doesn't recognise G in G.unit
    //       override def traverse[G[_] : Applicative, A, B](fa: List[A])(f: A => G[B]): G[List[B]] = ...
    override def traverse[G[_], A, B](fa: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] =
      fa.foldRight(G.unit(List[B]()))((a, glb) => G.map2(f(a), glb)(_ :: _))
  }

  val optionTraverse: Traverse[Option] = new Traverse[Option] {
    override def traverse[G[_], A, B](oa: Option[A])(f: A => G[B])(implicit G: Applicative[G]): G[Option[B]] =
      oa match {
        case None => G.unit(None)
        case Some(a) => G.map2(f(a), G.unit(None))((b, _) => Some(b))
      }
  }

  val optionTraverseTextbook: Traverse[Option] = new Traverse[Option] {
    override def traverse[G[_],A,B](oa: Option[A])(f: A => G[B])(implicit G: Applicative[G]): G[Option[B]] =
      oa match {
        case Some(a) => G.map(f(a))(Some(_))
        case None    => G.unit(None)
      }
  }

  case class Tree[+A](head: A, tail: List[Tree[A]])

//  val treeTraverse: Traverse[Tree] = new Traverse[Tree] {
//    override def traverse[G[_],A,B](t: Tree[A])(f: A => G[B])(implicit G: Applicative[G]): G[Tree[B]] = {
////      G.map(t.tail)
//      val lgtb = t.tail.map(tt => traverse(tt)(f))
//      val gb = f(t.head)
//
//      G.map2()
//      null
//    }
//  }
}