package fpinscala
package monads

import fpinscala.parallelism.nonblocking.Par
import fpinscala.parallelism.nonblocking.Par.Par
import fpinscala.parsing.take6.Parsers
import testing._
import state._

import language.higherKinds

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(a => Left(a))
    case Right(fb) => map(fb)(b => Right(b))
  }
}

object Functor {
  val listFunctor: Functor[List] = new Functor[List] {
    def map[A, B](as: List[A])(f: A => B): List[B] = as map f
  }
}

trait Monad[M[_]] extends Functor[M] {
  def unit[A](a: => A): M[A]

  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

  def map[A, B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A, B, C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    flatMap(ma) {
      a =>
        map(mb) {
          b => {
            f(a, b)
          }
        }
    }

  // Exercise 11.3
  // These implementations should be very similar to implementations from previous chapters,
  // only with more general types, and using the functions on the `Monad` trait. Make use
  // of `unit` and `map2`.
  def sequence[A](lma: List[M[A]]): M[List[A]] =
    lma.foldRight(unit(List.empty[A]))((ma, mla_acc) => {
      map2(ma, mla_acc)((a, b) => a :: b)
    })

  def traverseViaSequence[A, B](la: List[A])(f: A => M[B]): M[List[B]] =
    sequence(la map f)

  def traverse[A, B](la: List[A])(f: A => M[B]): M[List[B]] =
    la.foldRight(unit(List.empty[B]))((a, mlb) => map2(f(a), mlb)(_ :: _))

  /**
   * 'Balanced' sequencing, which should behave like `sequence`,
   * but it can use less stack for some data types. We'll see later
   * in this chapter how the monad _laws_ let us conclude both
   * definitions 'mean' the same thing.
   */
  def bsequence[A](ms: Seq[M[A]]): M[IndexedSeq[A]] = {
    if (ms.isEmpty) unit(Vector())
    else if (ms.size == 1) map(ms.head)(Vector(_))
    else {
      val (l, r) = ms.toIndexedSeq.splitAt(ms.length / 2)
      map2(bsequence(l), bsequence(r))(_ ++ _)
    }
  }

  // Exercise 11.4
  // Using standard list here

  // Exercise 11.5
  // General meaning of replicate is to exhaustively combine the values of A "n times", within the context of M

  // The general meaning of `replicateM` is described well by the implementation `sequence(List.fill(n)(ma))`.
  // It repeats the `ma` monadic value `n` times and gathers the results in a single value, where the monad `M`
  // determines how values are actually combined (as represented by implementation of map2)
  def replicateM[A](n: Int, ma: M[A]): M[List[A]] = {
    sequence(List.fill(n)(ma))
  }

  // Recursive version
  def _replicateM[A](n: Int, ma: M[A]): M[List[A]] =
    if (n <= 0) unit(List[A]()) else map2(ma, _replicateM(n - 1, ma))(_ :: _)

  def product[A, B](ma: M[A], mb: M[B]): M[(A, B)] = map2(ma, mb)((_, _))

  // Exercise 11.6
  // Hint: You can start by pattern matching on the argument. If the list is empty, our only choice is to return `unit(Nil)`
  def filterM[A](ms: List[A])(f: A => M[Boolean]): M[List[A]] = ms match {
    case Nil => unit(Nil)
    case h :: t =>
      val value = f(h)
      val value1 = filterM(t)(f)
      map2(value, value1)((b, la) => {
        val result = if (b) h :: la else la
        result
      })
  }

  // For `Par`, `filterM` filters a list, applying the functions in parallel
  // for `Option`, it filters a list, but allows the filtering function to fail and abort the filter computation
  // for `Gen`, it produces a generator for subsets of the input list, where the function `f` picks a 'weight'
  //            for each element (in the form of a `Gen[Boolean]`)
  def filterMTextbook[A](ms: List[A])(f: A => M[Boolean]): M[List[A]] =
    ms match {
      case Nil => unit(Nil)
      case h :: t => flatMap(f(h))(b =>
        if (!b) filterMTextbook(t)(f)
        else map(filterMTextbook(t)(f))(h :: _))
    }

  // Exercise 11.7
  def compose[A, B, C](f: A => M[B], g: B => M[C]): A => M[C] =
    a => flatMap(f(a))(g)

  // Exercise 11.8
  // Implement in terms of `compose`:
  def _flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] =
    compose[Unit, A, B]((_:Unit) => ma, f)()

  def _flatMapTextbook[A, B](ma: M[A])(f: A => M[B]): M[B] =
    compose((_:Unit) => ma, f)(())

  // Exercise 11.9
  // =============

  // You want to show that these two are equivalent:
  //
  // flatMap(flatMap(x)(f))(g) == flatMap(x)(a => flatMap(f(a))(g))
  // compose(compose(f, g), h) == compose(f, compose(g, h))
  //
  // Rewrite one in terms of the other.

  // compose(compose(f, g), h) == compose(f, compose(g, h))

  // a => flatMap(compose(f, g)(a))(h) == a => flatMap(f(a))(compose(g, h))

  // a => flatMap((b => flatMap(f(b))(g))(a))(h) == a => flatMap(f(a))(b => flatMap(g(b))(h))

  // So far we have just expanded the definition of `compose`. Equals substituted for equals.
  // Let's simplify the left side a little (via the inner flatMap)

  // a => flatMap(flatMap(f(a))(g))(h) == a => flatMap(f(a))(b => flatMap(g(b))(h))

  // Let's simplify again by eliminating the `a` argument and substituting a hypothetical
  // value `x` for `f(a)`:

  // flatMap(flatMap(x)(g))(h) == flatMap(x)(b => flatMap(g(b))(h))

  // This now looks exactly like the monad law stated in terms of `flatMap`, just with
  // different names:

  // flatMap(flatMap(x)(f))(g) == flatMap(x)(a => flatMap(f(a))(g))

  // i.e. substituting g = f, h = g, b = a, into

  // flatMap(flatMap(x)(g))(h) == flatMap(x)(b => flatMap(g(b))(h))

  // Results in:

  // flatMap(flatMap(x)(f))(g) == flatMap(x)(a => flatMap(f(a))(g))

  // Q.E.D.

  // Exercise 11.10
  // ==============

  // Law 1

  // def unit[A](a: => A): M[A]

  // def compose[A, B, C](f: A => M[B], g: B => M[C]): A => M[C] =
  //    a => flatMap(f(a))(g)

  // Law 1 is:

  // compose(f, unit) == f equiv. flatMap(x)(unit) = x

  // Substitute the definition of `compose` in terms of `flatMap` on LHS, having supplied v as argument i.e.

  // compose(f, unit)(v) == f(v)

  // (a => flatMap(f(a))(unit))(v) == f(v)

  // Substitute v for a

  // flatMap(f(v))(unit) == f(v)

  // Let x = f(v)

  // flatMap(x)(unit) == x, which is the RHS

  // Law 2 is:

  // compose(unit, f) == f equiv. flatMap(unit(y))(f) == f(y)

  // Substitute the definition of `compose` in terms of `flatMap` on LHS, having supplied v as argument i.e.

  // compose(unit, f)(v) == f(v)

  // (a => flatMap(unit(a))(f))(v) == f(v)

  // Substitute v for a

  // flatMap(unit(v))(f) == f(v)

  // Which is exactly the same as RHS, with v replaced with y

  // Needed for exercise 11.11 (text says we can ignore the distinction, but not here!)
  def unitEager[A](a: A): M[A] = unit(a)

  // Exercise 11.11
  // ==============

  // For `Option`, we again consider both cases `None` and `Some` and expand the equation.
  // The monadic `unit` is the `Some(_)` constructor.

  // First law:

  // flatMap(x)(unit) == x

  // (1) For x = None:

  // flatMap(None)(Some(_)) == None, OK

  // (2) For x = Some(v):

  // flatMap(Some(v))(Some(_)) == Some(v), OK

  // Second law:

  // flatMap(unit(y))(f) == f(y)

  // (1) For y = None:

  // flatMap(unit(None))(f) == f(None)

  // flatMap(None)(f) == f(None), OK

  // (2) For y = Some(v):

  // flatMap(unit(Some(v)))(f) == f(Some(v))

  // flatMap(Some(Some(v)))(f) == f(Some(v)), OK

  // Exercise 11.12
  def join[A](mma: M[M[A]]): M[A] =
    flatMap(mma)(ma => identity(ma))

  // Exercise 11.13
  // Implement flatMap in terms of join and map:

  // Join is sometimes called "flatten", and `flatMap` "maps and then flattens".
  def __flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] =
    join(map(ma)(f))

  // Implement compose in terms of join and map:
  def _compose[A, B, C](f: A => M[B], g: B => M[C]): A => M[C] =
    a => join(map(f(a))(g))

  // Exercise 11.14
  // Restate monad laws to mention only join, map, unit

  // First law

  // flatMap(x)(unit) == x

  // (1) join(map(x)(unit)) == x

  // Second law

  // flatMap(unit(y))(f) == f(y)

  // join(map(unit(y))(f)) == f(y)

  // Substitute identity function for f

  // (2) join(unit(y)) == y

  // Textbook answer
  // ===============

  // The identity laws in terms of `join` are:
  //
  // (1) join(map(x)(unit)) == x
  // (2) join(unit(x)) == x
  //
  // This follows from the definition of `join` and the identity laws in terms of `flatMap`:
  //
  // flatMap(x)(unit) == x
  // flatMap(unit(x))(f) == f(x)
  //
  // For the second law, we simply substitute the identity function for `f`, which gives us `join`.
  // (This is effectively the proof I went through above).

  // We can look at the associative law in terms of `flatMap` from another perspective. It says that
  // `x.flatMap(f).flatMap(g)` is equal to `x.flatMap(a => f(a).flatMap(g))` _for all_ choices of `f`
  // and `g`. So let's pick a particular `f` and `g` that's easy to think about.
  //
  // We can just pick the identity function:
  //
  // x.flatMap(z => z).flatMap(z => z) == x.flatMap(a => a.flatMap(z => z))
  //
  // And of course, flatMapping with the identify function is `join`! The associative law can now be stated as:
  //
  // join(join(x)) == x.flatMap(join)
  //
  // And we know that `flatMap` is "map, then join," so let's eliminate this last call to `flatMap`:
  //
  // join(join(x)) == join(map(x)(join))

  // Let's make a fast-and-loose proof for this version of the associative law using the `List` monad as an example.
  // Of course, `join` in the `List` monad is just _list concatenation_:
  //
  // scala> listMonad.join(List(List(1, 2), List(3, 4)))
  // res0: List[Int] = List(1, 2, 3, 4)
  //
  // Now let's say we have some lists, nested to a depth of three:
  //
  // val ns: List[List[List[Int]]] =
  //   List(List(List(1,2), List(3,4)),
  //        List(List(), List(5))
  //   )
  //
  // If we `join` this list, the outer lists get concatenated and we have a list of lists two levels deep:
  //
  // scala> ns.flatten
  // res1: List[List[Int]] = List(List(1, 2), List(3, 4), List(), List(5))
  //
  // If we instead _map_ `join` over it, we get a different nesting but again two levels deep.
  // This flattens the _inner_ lists.
  //
  // scala> ns.map(listMonad.join)
  // res2: List[List[Int]] = List(List(1, 2, 3, 4), List(5))
  //
  // And then joining `res1` should be the same as joining `res2`:
  //
  // scala> listMonad.join(res1) == listMonad.join(res2)
  // res3: Boolean = true
  //
  // So all that the associative law is saying for the `List` monad is that concatenating the outer lists and
  // then the inner ones (`join(join(ns))`) is the same as first concatenating the inner lists and then the
  // outer ones (`join(ns.map(join))`).

  // Exercise 11.15, Textbook Answer

  // We can state the associative law in terms of `join`:

  // join(join(x)) == join(map(x)(join))
  //
  // For `Par`, the `join` combinator means something like "make the outer thread wait for the inner one to finish."
  // What this law is saying is that if you have threads starting threads three levels deep, then joining the inner
  // threads and then the outer ones is the same as joining the outer threads and then the inner ones.
  //
  // For `Parser`, the `join` combinator is running the outer parser to produce a `Parser`, then running the inner
  // `Parser` _on the remaining input_. The associative law is saying, roughly, that only the _order_ of nesting
  // matters, since that's what affects the order in which the parsers are run.

  // Exercise 11.16 - textbook answer
  // Recall the identity laws:
  //
  // left identity:  flatMap(unit(x))(f) == f(x)
  // right identity: flatMap(x)(unit)    == x
  //
  // The left identity law for `Gen`:
  // The law states that if you take the values generated by `unit(x)` (which are always `x`) and apply `f` to those
  // values, that's exactly the same as the generator returned by `f(x)`.
  //
  // The right identity law for `Gen`:
  // The law states that if you apply `unit` to the values inside the generator `x`, that does not in any way differ
  // from `x` itself.
  //
  // The left identity law for `List`:
  // The law says that wrapping a list in a singleton `List` and then flattening the result is the same as
  // doing nothing. (f is the identity function in this case)
  //
  // The right identity law for `List`:
  // The law says that if you take every value in a list, wrap each one in a singleton `List`, and then
  // flatten the result, you get the list you started with.
}

object Monad {
  // Exercise 11.17
  case class Id[A](value: A) {
    def map[B](f: A => B): Id[B] = Id(f(value))
    def flatMap[B](f: A => Id[B]): Id[B] = f(value)
  }

  val idMonad: Monad[Id] = new Monad[Id] {
    override def unit[A](a: => A): Id[A] = Id(a)
    override def flatMap[A, B](ida: Id[A])(f: A => Id[B]): Id[B] = ida flatMap f
  }

  val genMonad: Monad[Gen] = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)

    override def flatMap[A, B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
      ma flatMap f
  }

  // Exercise 11.1
  val parMonad: Monad[Par] = new Monad[Par] {
    override def unit[A](a: => A): Par[A] = Par.unit(a)

    override def flatMap[A, B](ma: Par[A])(f: A => Par[B]): Par[B] = Par.flatMap(ma)(f)
  }

  def parserMonad[P[+_]](p: Parsers[P]): Monad[P] = new Monad[P] {
    override def unit[A](a: => A): P[A] = p.succeed(a)

    override def flatMap[A, B](ma: P[A])(f: A => P[B]): P[B] = p.flatMap(ma)(f)
  }

  val optionMonad: Monad[Option] = new Monad[Option] {
    override def unit[A](a: => A): Option[A] = Some(a)

    override def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] = ma.flatMap(f)
  }

  import fpinscala.laziness.Stream

  val streamMonad: Monad[Stream] = new Monad[Stream] {
    override def unit[A](a: => A): Stream[A] = Stream(a)

    override def flatMap[A, B](ma: Stream[A])(f: A => Stream[B]): Stream[B] = ma.flatMapTextbook(f)
  }

  val listMonad: Monad[List] = new Monad[List] {
    override def unit[A](a: => A): List[A] = List(a)

    override def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] = ma.flatMap(f)
  }

  // Exercise 11.2

  // Hint: Since `State` is a binary type constructor, we need to partially apply it with the `S` type argument. Thus,
  // it is not just one monad, but an entire family of monads, one for each type `S`. You need to devise a way of
  // capturing the type `S` in a type-level scope and providing a partially applied `State` type in that scope.

  // def stateMonad[S] = ???

  // Answer is discussed later...

  // Section 11.5.2

  type IntState[A] = State[Int, A]

  val IntStateMonad = new Monad[IntState] {
    override def unit[A](a: => A): IntState[A] = State(s => (a, s))

    override def flatMap[A, B](st: IntState[A])(f: A => IntState[B]): IntState[B] = st flatMap f
  }

  object _IntStateMonad extends Monad[({type IntState[A] = State[Int,A]})#IntState] {
    // def unit[A](a: => A): M[A]
    // def unit[A](a: => A): IntState[A]
    override def unit[A](a: => A): State[Int, A] = State(i => (a, i))

    // def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
    // def flatMap[A, B](ma: IntState[A])(f: A => IntState[B]): IntState[B]
    override def flatMap[A, B](st: State[Int, A])(f: A => State[Int, B]): State[Int, B] = st flatMap f
  }

  // A type constructor declared inline as above is often called a type lambda in Scala. This trick
  // can be used to partially apply the State type constructor and declare a StateMonad trait. An
  // instance of StateMonad[S] is then a monad instance for the given state type S:

  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    // def unit[A](a: => A): M[A]
    override def unit[A](a: => A): State[S, A] = State(s => (a, s))

    // def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
    override def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] = st flatMap f
  }

  // Exercise 11.19

  // identity & associativity (as per monad) => Not discussed in answer
  // set state (get state) = start state     => Matched by part 1 below

  // Textbook answer:

  // Getting and setting the same state does nothing:

  // getState.flatMap(setState) == unit(())
  //
  // written as for-comprehension:

  // for {
  //   x <- getState
  //   _ <- setState(x)
  // } yield ()
  //
  // Setting the state to `s` and getting it back out yields `s`.

  // setState(s).flatMap(_ => getState) == unit(s)
  //
  // alternatively:

  // for {
  //   _ <- setState(s)
  //   x <- getState
  // } yield x

  // Exercise 11.20
  case class Reader[R, A](run: R => A)

  // Hint: This monad is very similar to the `State` monad, except that it's "read-only".
  // You can "get" but not "set" the `R` value that `flatMap` carries along.
  def readerMonad[R] = new Monad[({type f[x] = Reader[R, x]})#f] {
    def unit[A](a: => A): Reader[R, A] = Reader(_ => a)

    override def flatMap[A, B](st: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] =
      Reader(r => f(st.run(r)).run(r))
  }

  // A primitive operation for it would be simply to ask for the `R` argument:
  def ask[R]: Reader[R, R] = Reader(r => r)
}

object MonadLaws {
  import fpinscala.testing.take2.Gen

  case class Order(item: Item, quantity: Int)
  case class Item(name: String, price: Double)

  val genOrder: Gen[Order] = for {
    name <- Gen.stringGen(3)
    price <- Gen.choose(1, 10)
    quantity <- Gen.choose(1, 100)
  } yield Order(Item(name, price), quantity)

  val genItem: Gen[Item] = for {
    name <- Gen.stringGen(3)
    price <- Gen.choose(1, 10)
  } yield Item(name, price)

  val genOrder2: Gen[Order] = for {
    item <- genItem
    quantity <- Gen.choose(1, 100)
  } yield Order(item, quantity)
}
