package fpinscala.parallelism.blocking

import java.util.concurrent._
import scala.annotation.tailrec
import scala.language.implicitConversions

// NOTE - This version of Par doesn't work, not 100% sure why e.g. call from unit never returns
object ParBroken {
  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  //noinspection ScalaUnusedSymbol
  // `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation of `Future`
  // that just wraps a constant value. It doesn't use the `ExecutorService` at all. It's always done and can't be
  // cancelled. Its `get` method simply returns the value that we gave it.
  def unit[A](a: A): Par[A] = _ => UnitFuture(a)

  private case class UnitFuture[A](a: A) extends Future[A] {
    def isDone = true

    def get(timeout: Long, units: TimeUnit): A = get()

    def get(): A = a

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having
  // `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we
  // want the evaluation of `f` to occur in a separate thread.

  // This implementation of `map2` does _not_ respect timeouts, and eagerly waits for the returned futures.
  // This means that even if you have passed in "forked" arguments, using this map2 on them will make them wait.
  // It simply passes the `ExecutorService` on to both `Par` values, waits for the results of the Futures
  // `af` and `bf`, applies `f` to them, and wraps them in a `UnitFuture`. In order to respect timeouts, we'd
  // need a new `Future` implementation that records the amount of time spent evaluating `af`, then subtracts
  // that time from the available time allocated for evaluating `bf`.
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af: Future[A] = a(es)
      val bf: Future[B] = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  // This is the simplest and most natural implementation of `fork`, but there are some problems with it--for one,
  // the outer `Callable` will block waiting for the "inner" task to complete. Since this blocking occupies a
  // thread in our thread pool, or whatever resource backs the `ExecutorService`, this implies that we're losing
  // out on some potential parallelism. Essentially, we're using two threads when one should suffice. This is a
  // symptom of a more serious problem with the implementation, and we will discuss this later in the chapter.
  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call: A = a(es).get
    })

  // Exercise 7.3
  /* This version respects timeouts. See `Map2Future` below. */
  def map2WithTimeouts[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
  es => {
    val (af, bf) = (a(es), b(es))
    Map2Future(af, bf, f)
  }

  /*
  Note: this implementation will not prevent repeated evaluation if multiple threads call `get` in parallel.
  We could prevent this using synchronization, but it isn't needed for our purposes here (also, repeated
  evaluation of pure values won't affect results).
  */
  case class Map2Future[A, B, C](a: Future[A], b: Future[B],
                                 f: (A, B) => C) extends Future[C] {
    @volatile var cache: Option[C] = None

    def isDone: Boolean = cache.isDefined

    def isCancelled: Boolean = a.isCancelled || b.isCancelled

    def cancel(evenIfRunning: Boolean): Boolean =
      a.cancel(evenIfRunning) || b.cancel(evenIfRunning)

    def get: C = compute(Long.MaxValue)

    def get(timeout: Long, units: TimeUnit): C =
      compute(TimeUnit.NANOSECONDS.convert(timeout, units))

    private def compute(timeoutInNanos: Long): C = cache match {
      case Some(c) => c
      case None =>
        val start = System.nanoTime
        val ar = a.get(timeoutInNanos, TimeUnit.NANOSECONDS)
        val stop = System.nanoTime
        val aTime = stop - start
        val br = b.get(timeoutInNanos - aTime, TimeUnit.NANOSECONDS)
        val ret = f(ar, br)
        cache = Some(ret)
        ret
    }
  }

  // Exercise 7.4
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] = {
    a => lazyUnit(f(a))
  }

  def parSort(parList: Par[List[Int]]): Par[List[Int]] = {
    map2(parList, unit(()))((a, _) => a.sorted)
  }

  // Generalising to map
  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def parSort2(parList: Par[List[Int]]): Par[List[Int]] = map(parList)(_.sorted)

  // Exercise 7.5 - write sequence
  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
    @tailrec
    def go(l: List[Par[A]], acc: Par[List[A]]): Par[List[A]] = {
      l match {
        case h :: t => go(t, map2(acc, h)((a, b) => a :+ b))
        case _ => acc
      }
    }

    go(ps, unit(List.empty[A]))
  }

  def sequence2[A](ps: List[Par[A]]): Par[List[A]] = ps match {
    case h :: t => map2(h, sequence2(t))((a, la) => la :+ a)
    case _ => unit(List.empty[A])
  }

  // Exercise 7.5 - textbook answers
  def sequence_simpleTextbook[A](l: List[Par[A]]): Par[List[A]] =
    l.foldRight[Par[List[A]]](unit(List()))((h, acc) => map2(h, acc)(_ :: _))

  // This implementation forks the recursive step off to a new logical thread,
  // making it effectively tail-recursive. However, we are constructing
  // a right-nested parallel program, and we can get better performance by
  // dividing the list in half, and running both halves in parallel.
  // See `sequenceBalanced` below.

  // NOTE: This is a very close match for sequence above
  def sequenceRightTextbook[A](as: List[Par[A]]): Par[List[A]] =
    as match {
      case Nil => unit(Nil)
      case h :: t => map2(h, fork(sequenceRightTextbook(t)))(_ :: _)
    }

  // We define `sequenceBalanced` using `IndexedSeq`, which provides an
  // efficient function for splitting the sequence in half.
  def sequenceBalancedTextbook[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
    if (as.isEmpty) unit(Vector())
    else if (as.length == 1) map(as.head)(a => Vector(a))
    else {
      val (l, r) = as.splitAt(as.length / 2)
      map2(sequenceBalancedTextbook(l), sequenceBalancedTextbook(r))(_ ++ _)
    }
  }

  def sequenceTextbook[A](as: List[Par[A]]): Par[List[A]] =
    map(sequenceBalancedTextbook(as.toIndexedSeq))(_.toList)

  // End of exercise 7.5

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs = ps.map(asyncF(f))
    sequenceTextbook(fbs)
  }

  // Exercise 7.6
  def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars: List[Par[List[A]]] =
      l map asyncF((a: A) => if (f(a)) List(a) else List())
    map(sequence(pars))(_.flatten) // convenience method on `List` for concatenating a list of lists
  }

  // Can also do using Option (which I had actually thought of :)
  def parFilter2[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars: List[Par[Option[A]]] = l map asyncF((a: A) => Some(a).filter(f))
    map(sequence(pars))(_.flatten)
  }

  // Exercise 7.6 extension exercises

  // (1) Is there a more general version of the parallel summation function? Try using it to find the maximum
  //     value of an IndexedSeq in parallel

  // original version
  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1) {
      unit(ints.headOption getOrElse 0)
    } else {
      val (l, r) = ints.splitAt(ints.length / 2)
      ParBroken.map2(ParBroken.sum(l), ParBroken.sum(r))(_ + _)
    }

  // sum generalised in terms of arguments
  def fold1(ints: IndexedSeq[Int])(z: Int)(f: (Int, Int) => Int): Par[Int] = {
  // TODO - Fix!
  // TODO This should work, note the change from size <= 1
    if (ints.isEmpty) {
      unit(z)
    } else {
      val (l, r) = ints.splitAt(ints.length / 2)
      ParBroken.map2(ParBroken.fold1(l)(z)(f), ParBroken.fold1(r)(z)(f))(f)
    }
  }

  // fold1 generalised in terms of types
  def fold2[A, B](items: IndexedSeq[A])(z: B)(f: (B, B) => B): Par[B] =
  // TODO - Fix!
  // TODO This should work, note the change from size <= 1
    if (items.isEmpty) {
      unit(z)
    } else {
      val (l, r) = items.splitAt(items.length / 2)
      ParBroken.map2(ParBroken.fold2(l)(z)(f), ParBroken.fold2(r)(z)(f))(f)
    }

  // sum implemented in terms of folds
  def sumViaFold1(ints: IndexedSeq[Int]): Par[Int] =
    fold1(ints)(0)(_ + _)

  def sumViaFold2(ints: IndexedSeq[Int]): Par[Int] =
    fold2(ints)(0)(_ + _)

  // max value in terms of folds
  def maxValueViaFold1(ints: IndexedSeq[Int]): Par[Int] =
    fold1(ints)(Int.MinValue)((a, b) => if (a > b) a else b)

  def maxValueViaFold2(ints: IndexedSeq[Int]): Par[Int] =
    fold2(ints)(Int.MinValue)((a, b) => if (a > b) a else b)

  // (2) Write a function that takes a list of paragraphs (a List[String]) and returns
  //     the total number of words across all paragraphs in parallel
  //     Generalise the function as much as possible
  def wc1(input: List[String]): Int = {
    input.foldLeft(0)((a, b) => a + b.split(" ").length)
  }

  // First parallel version
  def wc2(input: List[String]): Par[Int] = {
    if (input.size <= 1)
      unit(input.headOption.map(_.split(" ").length).getOrElse(0))
    else {
      val (l, r) = input.splitAt(input.length / 2)
      ParBroken.map2(ParBroken.wc2(l), ParBroken.wc2(r))(_ + _)
    }
  }

  // Generalised on fn arguments
  def wc3(input: List[String])(f: Option[String] => Int)(g: (Int, Int) => Int): Par[Int] = {
    if (input.size <= 1)
      unit(f(input.headOption))
    else {
      val (l, r) = input.splitAt(input.length / 2)
      ParBroken.map2(ParBroken.wc3(l)(f)(g), ParBroken.wc3(r)(f)(g))(g)
    }
  }

  // Further generalised on type
  def wc4[A, B](input: List[A])(f: Option[A] => B)(g: (B, B) => B): Par[B] = {
    if (input.size <= 1)
      unit(f(input.headOption))
    else {
      val (l, r) = input.splitAt(input.length / 2)
      ParBroken.map2(ParBroken.wc4(l)(f)(g), ParBroken.wc4(r)(f)(g))(g)
    }
  }

  // Parallel word count in terms of generalised function
  def wordCount(input: List[String]): Par[Int] = {
    wc4(input)(_.map(_.split(" ").length).getOrElse(0))(_ + _)
  }

  // (3) Implement map3, map4 and map5 in terms of map2
  //     Signature of map2 is:
  //     def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C]

  def map3[A, B, C, D](a: Par[A], b: Par[B], c: Par[C])(f: (A, B, C) => D): Par[D] = {
    map2(map2(a, b)((a, b) => f.curried(a)(b)), c)((fcd, c) => fcd(c))
  }

  def map4[A, B, C, D, E](a: Par[A], b: Par[B], c: Par[C], d: Par[D])(f: (A, B, C, D) => E): Par[E] = {
    map2(map2(map2(a, b)((a, b) => f.curried(a)(b)), c)((fcd, c) => fcd(c)), d)((fde, d) => fde(d))
  }

  def map5[A, B, C, D, E, F](a: Par[A], b: Par[B], c: Par[C], d: Par[D], e: Par[E])(f: (A, B, C, D, E) => F): Par[F] = {
    map2(
      map2(
        map2(
          map2(a, b)((a, b) => f.curried(a)(b)),
          c
        )((fcd, c) => fcd(c)),
        d
      )((fde, d) => fde(d)),
      e
    )((fef, e) => fef(e))
  }

  // Exercise 7.7
  /*
  map(y)(id) == y

  map(map(y)(g))(f) == map(y)(f compose g)

  Sub id for g

  map(map(y)(id))(f) == map(y)(f compose id)

  map(y)(f) == map(y)(f)
  */

  // Exercise 7.9

  /*
  Given following definition of fork, show that any fixed-size thread pool can be made to deadlock

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })
  */

  // Thread pool size 1
  val a: Par[Int] = lazyUnit(42 + 1)
  val S: ExecutorService = Executors.newFixedThreadPool(1)
  val x: Boolean = ParBroken.equal(S)(a, fork(a))

  // Thread pool size 2
  val T: ExecutorService = Executors.newFixedThreadPool(2)
  val y: Boolean = ParBroken.equal(S)(a, fork(fork(a)))

  // and so on....

  // Textbook answer:

  // For a thread pool of size 2, `fork(fork(fork(x)))` will deadlock, and so on. Another, perhaps more interesting
  // example is `fork(map2(fork(x), fork(y)))`. In this case, the outer task is submitted first and occupies a thread
  // waiting for both `fork(x)` and `fork(y)`. The `fork(x)` and `fork(y)` tasks are submitted and run in parallel,
  // except that only one thread is available, resulting in deadlock.

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es =>
      if (run(es)(cond).get) t(es) // Notice we are blocking on the result of `cond`.
      else f(es)

  // Exercise 7.11
  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = {
    es =>
      val index = run(es)(n).get % choices.size
      choices(index)(es)
  }

  def choiceViaChoiceN[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN(ParBroken.map(cond)(b => if (b) 0 else 1))(List(t, f))

  // Exercise 7.12
  def choiceMap[K,V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    es => {
      val actualKey = run(es)(key).get
      run(es)(choices(actualKey))
    }

  // Exercise 7.13
  def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] = {
    es =>
      val a = run(es)(pa).get
      run(es)(choices(a))
  }

  def choiceViaChooser[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    chooser(cond)(a => if (a) t else f)

  def choiceNViaChooser[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    chooser(n)(choice => choices(choice % choices.size))

  // Exercise 7.14
  def join[A](a: Par[Par[A]]): Par[A] = {
    es => run(es)(run(es)(a).get())
  }

  def flatMap[A, B](p: Par[A])(f: A => Par[B]): Par[B] = {
    join(map(p)(f))
  }

  def joinViaFlatmap[A](a: Par[Par[A]]): Par[A] = {
    flatMap(a)(identity)
  }

  // This implementation is not safe for execution on bounded thread pools, and it also does not preserve timeouts.
  // Can you see why? You may wish to try implementing a nonblocking version like was done for `fork`.
  def joinAlternative[A](a: Par[Par[A]]): Par[A] =
    es => a(es).get.apply(es)

  // Extensions
  // (1) Can you implement a function with the same signature as map2, but using flatMap and unit?
  //     How is its meaning different than that of map2?

  def map2Alt[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    flatMap(a)(a => flatMap(b)(b => unit(f(a, b))))

  def parSort3(parList: Par[List[Int]]): Par[List[Int]] = {
    map2Alt(parList, unit(()))((a, _) => a.sorted)
  }
}

object Examples {

  // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists,
  // these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
  def sum(ints: IndexedSeq[Int]): Int =
    if (ints.size <= 1)
    // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
      ints.headOption getOrElse 0
    else {
      // Divide the sequence in half using the `splitAt` function.
      val (l, r) = ints.splitAt(ints.length / 2)
      // Recursively sum both halves and add the results together.
      sum(l) + sum(r)
    }

  def main(args: Array[String]): Unit = {
    println(List(1, 2).splitAt(1))
    println(List(1).splitAt(0))
    println(List(1).splitAt(1))
  }
}