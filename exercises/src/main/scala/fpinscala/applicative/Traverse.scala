package fpinscala.applicative

import fpinscala.monads.Functor
import fpinscala.monoids.{Foldable, Monoid}
import fpinscala.state.State
import fpinscala.state.State.{get, set}

import scala.language.{higherKinds, implicitConversions}

//noinspection NotImplementedCode,ScalaUnusedSymbol
trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  // NOTE - This definition is useful to a point, but one of these methods must be overridden for a traversible
  //        otherwise it just recurses forever (also, it would not make sense as we need to call methods on G to
  //        leverage the fact that G is an applicative).

  //        Note that in exercise 12.13, the textbook answer overrides traverse in each case
  // TODO - Check https://stackoverflow.com/questions/4465948/what-are-scala-context-and-view-bounds/4467012#4467012
  def traverse[G[_] : Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence(map(fa)(f))

  def sequence[G[_] : Applicative, A](fga: F[G[A]]): G[F[A]] = {
    // Substitute into traverse
    // F[A]      becomes F[G[A]        i.e. A becomes G[A]
    // A => G[B] becomes G[A] => G[A]  i.e. B becomes A
    // So traverse returns G[F[B]] i.e. G[F[A]]
    traverse(fga)(ga => ga)
  }

  // Exercise 12.14 - implement map in terms of traverse

  import fpinscala.applicative.applicativevsmonad.OptionApplicative.F

  implicit val optionApplicative: Applicative[Option] = F

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    traverse[Option, A, B](fa)(a => Some(f(a))).get

  // Textbook answer

  // The simplest possible `Applicative` we can use is `Id`:

  type Id[A] = A

  // We already know this forms a `Monad`, so it's also an applicative functor:
  // This statement seems to be based on the Id class that appeared in chapter 11,
  // although in that case it was a case class i.e.

  // case class Id[A](value: A) {
  //    def map[B](f: A => B): Id[B] = Id(f(value))
  //    def flatMap[B](f: A => Id[B]): Id[B] = f(value)
  //  }
  //
  // val idMonad: Monad[Id] = new Monad[Id] {
  //    override def unit[A](a: => A): Id[A] = Id(a)
  //    override def flatMap[A, B](ida: Id[A])(f: A => Id[B]): Id[B] = ida flatMap f
  // }

  // In this case we have:

  implicit val idMonad: Monad[Id] = new Monad[Id] {
    override def unit[A](a: => A): Id[A] = a

    override def flatMap[A, B](ida: Id[A])(f: A => Id[B]): Id[B] = f(ida)
  }

  // We can now implement map by calling traverse, picking Id as the Applicative:

  // Making idMonad implicit above fulfils the criteria required to make the call to traverse work i.e.
  // that there is an implicit Applicative[Id] as definition of Monad is:

  // trait Monad[F[_]] extends Applicative[F]

  // def traverse[G[_] : Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

  def _map[A, B](fa: F[A])(f: A => B): F[B] =
    traverse[Id, A, B](fa)(f)

  // This implementation is suggestive of laws for `traverse`, since we expect this implementation to obey
  // the usual functor laws. See the chapter notes for discussion of the laws for `Traverse`.
  //
  // Note that we can define `traverse` in terms of `sequence` and `map` (as it has been above).
  // This means that a valid `Traverse` instance may define `sequence` and `map`, or just `traverse`:
  //
  // trait Traverse[F[_]] extends Functor[F] {
  //   def traverse[G[_]:Applicative,A,B](fa: F[A])(f: A => G[B]): G[F[B]] =
  //     sequence(map(fa)(f))
  //   def sequence[G[_]:Applicative,A](fga: F[G[A]]): G[F[A]] =
  //     traverse(fga)(ga => ga)
  //   def map[A,B](fa: F[A])(f: A => B): F[B] =
  //     traverse[Id, A, B](fa)(f)(idMonad)
  // }

  // Section 12.7.1 - From monoids to applicative functors (after exercise 12.14)
  // Discards second type
  type Const[M, B] = M

  // See https://underscore.io/blog/posts/2016/12/05/type-lambdas.html
  implicit def monoidApplicative[M](M: Monoid[M]): Applicative[({type f[x] = Const[M, x]})#f] =
    new Applicative[({type f[x] = Const[M, x]})#f] {
      // def unit[A](a: => A): F[A]
      // (1) Substitute: type f[x] = Const[M, x]
      // def unit[A](a: => A): Const[M, A]
      // (2) Substitute: type Const[M, B] = M
      override def unit[A](a: => A): M = M.zero

      // def apply[A, B](fab: F[A => B])(fa: F[A]): F[B]
      // (1) Substitute: type f[x] = Const[M, x]
      // def apply[A, B](fab: Const[M, A => B])(fa: Const[M, A]): Const[M, B]
      // (2) Substitute: type Const[M, B] = M
      // def apply[A, B](fab: M)(fa: M): M
      override def apply[A, B](m1: M)(m2: M): M = M.op(m1, m2)

      // Similarly for map2
      // def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = ???
      // Note that f isn't used here
      override def map2[A, B, C](m1: M, m2: M)(f: (A, B) => C): M = M.op(m1, m2)
    }

  // This means that Traverse can extend Foldable and we can give a default
  // implementation of foldMap in terms of traverse:
  override def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    traverse[({type f[x] = Const[B, x]})#f, A, Nothing](as)(f)(monoidApplicative(mb))

  def _foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B = {
    implicit def _monoidApplicative: Applicative[({type f[x] = Const[B, x]})#f] = monoidApplicative(mb)
    // def traverse[G[_] : Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]
    // def traverse[({type f[x] = Const[B, x]})#f, A, Nothing](fa: F[A])(f: A => G[B]): G[F[B]]
    // def traverse[({type f[x] = Const[B, x]})#f, A, Nothing](fa: F[A])(f: A => G[Nothing]): G[F[Nothing]]
    // def traverse[({type f[x] = Const[B, x]})#f, A, Nothing](fa: F[A])(f: A => Const[B, Nothing]): Const[B, F[Nothing]]
    // def traverse[({type f[x] = Const[B, x]})#f, A, Nothing](fa: F[A])(f: A => B): B

    traverse[({type f[x] = Const[B, x]})#f, A, Nothing](as)(f)
  }

  // Section 12.7.2 - Traversal with state
  def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] = {
    // def traverseS[G[_] : Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]
    // Substitute type f[x] = State[S, x] for G[]
    // def traverseS[G[_] : Applicative, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]]
    traverse[({type f[x] = State[S, x]})#f, A, B](fa)(f)(Monads.stateMonad)
  }

  // Label every element with its position
  def _zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    traverseS(fa)((a: A) => for {
      i <- get[Int]
      _ <- set(i + 1)
    } yield (a, i)).run(0)._1

  def _toList[A](fa: F[A]): List[A] =
    traverseS(fa)((a: A) => for {
      as <- get[List[A]]
      _ <- set(a :: as)
    } yield ()).run(Nil)._2.reverse

  // Factoring out common code
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

  // Exercise 12.16
  // Hint: We need to use a stack. Fortunately a `List` is the same thing as a stack, and we already know how to
  //       turn any traversable into a list!
  def reverse[A](fa: F[A]): F[A] = {
    val reversedList = toList(fa).reverse
    mapAccum(fa, reversedList)((_, as) => (as.head, as.tail))._1
  }

  // Exercise 12.17
  // def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S)
  // Hint: This implementation is very similar to `toList` except instead
  //       of accumulating into a list, we are accumulating into a `B`
  //       using the `f` function.
  def _foldLeft[A, B](fa: F[A])(z: B)(f: (B, A) => B): (F[B], B) =
    mapAccum(fa, z)((a, b) => (f(b, a), f(b, a)))

  override def foldLeft[A, B](fa: F[A])(z: B)(f: (B, A) => B): B =
    mapAccum(fa, z)((a, b) => ((), f(b, a)))._2

  // Section 12.7.3 - Combining traversable structures
  def zip[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    mapAccum(fa, toList(fb)) {
      case (a, Nil) => sys.error("zip: Incompatible shapes.")
      case (a, b :: bs) => ((a, b), bs)
    }._1

  def zipL[A, B](fa: F[A], fb: F[B]): F[(A, Option[B])] =
    mapAccum(fa, toList(fb)) {
      case (a, Nil) => ((a, None), Nil)
      case (a, b :: bs) => ((a, Some(b)), bs)
    }._1

  def zipR[A, B](fa: F[A], fb: F[B]): F[(Option[A], B)] =
    mapAccum(fb, toList(fa)) {
      case (b, Nil) => ((None, b), Nil)
      case (b, a :: as) => ((Some(a), b), as)
    }._1

  def fuse[G[_], H[_], A, B](fa: F[A])(f: A => G[B], g: A => H[B])
                            (implicit G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) = ???

  def compose[G[_]](implicit G: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f] = ???
}

object Traverse {
  // Exercise 12.13
  val listTraverse: Traverse[List] = new Traverse[List] {
    // (1) Version with implicit parameter
    // override def traverse[G[_], A, B](fa: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] =
    //   fa.foldRight(G.unit(List[B]()))((a, glb) => G.map2(f(a), glb)(_ :: _))

    // (2) Version with implicitly
    // override def traverse[G[_] : Applicative, A, B](fa: List[A])(f: A => G[B]): G[List[B]] =
    //  fa.foldRight(implicitly[Applicative[G]].unit(List[B]()))((a, glb) => implicitly[Applicative[G]].map2(f(a), glb)(_ :: _))

    // (3) Version using syntactic sugar declared on Applicative companion object
    override def traverse[G[_] : Applicative, A, B](fa: List[A])(f: A => G[B]): G[List[B]] =
      fa.foldRight(Applicative[G].unit(List[B]()))((a, glb) => Applicative[G].map2(f(a), glb)(_ :: _))
  }

  val optionTraverse: Traverse[Option] = new Traverse[Option] {
    // (1) Version with implicit parameter
    // override def traverse[G[_], A, B](oa: Option[A])(f: A => G[B])(implicit G: Applicative[G]): G[Option[B]] =
    //   oa match {
    //     case None => G.unit(None)
    //     case Some(a) => G.map2(f(a), G.unit(None))((b, _) => Some(b))
    //   }

    override def traverse[G[_] : Applicative, A, B](oa: Option[A])(f: A => G[B]): G[Option[B]] =
      oa match {
        case None => Applicative[G].unit(None)
        case Some(a) => Applicative[G].map2(f(a), Applicative[G].unit(None))((b, _) => Some(b))
      }
  }

  val optionTraverseTextbook: Traverse[Option] = new Traverse[Option] {
    override def traverse[G[_] : Applicative, A, B](oa: Option[A])(f: A => G[B]): G[Option[B]] =
      oa match {
        case Some(a) => Applicative[G].map(f(a))(Some(_))
        case None => Applicative[G].unit(None)
      }
  }

  case class Tree[+A](head: A, tail: List[Tree[A]])

  val treeTraverse: Traverse[Tree] = new Traverse[Tree] {
    override def traverse[G[_] : Applicative, A, B](t: Tree[A])(f: A => G[B]): G[Tree[B]] = {
      val gb: G[B] = f(t.head)

      val ta2gtb: Tree[A] => G[Tree[B]] = treeA => traverse(treeA)(f)
      val gltb: G[List[Tree[B]]] = listTraverse.traverse(t.tail)(ta2gtb)

      Applicative[G].map2(gb, gltb)(Tree(_, _))
    }
  }
}