package fpinscala.parallelism

import java.util.concurrent._
import scala.annotation.tailrec
import scala.language.implicitConversions

class CompletableOnGetFuture[A](completedValue: A) extends CompletableFuture[A] {
  override def get(): A = {
    complete(completedValue)
    super.get()
  }
}

object ParCompletableFuture {
  val NO_OF_THREADS: Int = 10

  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = {
    _ => new CompletableOnGetFuture(a)
  }

  def run[A](es: ExecutorService)(a: Par[A]): Future[A] = {
    es.submit(new Callable[A] {
      def call: A = a(es).get
    })
  }

  // NOTE: This version of fork blocks
  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call: A = a(es).get
    })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  // NOTE: This version of map2 blocks
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = {
    (es: ExecutorService) => {
      val af: Future[A] = a(es)
      val bf: Future[B] = b(es)
      val aVal = af.get
      val bVal = bf.get
      unit(f(aVal, bVal))(es)
    }
  }

  def parSort(parList: Par[List[Int]]): Par[List[Int]] = {
    parList.map2(unit(()))((a, _) => a.sorted)
  }

  // Generalising to map
  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    pa.map2(unit(()))((a, _) => f(a))

  def parSort2(parList: Par[List[Int]]): Par[List[Int]] = parList.map(_.sorted)

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
    @tailrec
    def go(l: List[Par[A]], acc: Par[List[A]]): Par[List[A]] = {
      l match {
        case h :: t => go(t, acc.map2(h)((a, b) => a :+ b))
        case _ => acc
      }
    }

    go(ps, unit(List.empty[A]))
  }

  def sequence2[A](ps: List[Par[A]]): Par[List[A]] = ps match {
    case h :: t => h.map2(sequence2(t))((a, la) => la :+ a)
    case _ => unit(List.empty[A])
  }

  def sequence_simpleTextbook[A](l: List[Par[A]]): Par[List[A]] =
    l.foldRight[Par[List[A]]](unit(List()))((h, acc) => h.map2(acc)(_ :: _))

  def sequenceRightTextbook[A](as: List[Par[A]]): Par[List[A]] =
    as match {
      case Nil => unit(Nil)
      case h :: t => h.map2(fork(sequenceRightTextbook(t)))(_ :: _)
    }

  def sequenceBalancedTextbook[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
    if (as.isEmpty) unit(Vector())
    else if (as.length == 1) as.head.map(a => Vector(a))
    else {
      val (l, r) = as.splitAt(as.length / 2)
      sequenceBalancedTextbook(l).map2(sequenceBalancedTextbook(r))(_ ++ _)
    }
  }

  def sequenceTextbook[A](as: List[Par[A]]): Par[List[A]] =
    sequenceBalancedTextbook(as.toIndexedSeq).map(_.toList)

  def asyncF[A, B](f: A => B): A => Par[B] = {
    a => lazyUnit(f(a))
  }

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs = ps.map(asyncF(f))
    sequenceTextbook(fbs)
  }

  def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars: List[Par[List[A]]] =
      l map asyncF((a: A) => if (f(a)) List(a) else List())
    // convenience method on `List` for concatenating a list of lists
    sequence(pars).map(_.flatten)
  }

  // Can also do using Option (which I had actually thought of :)
  def parFilter2[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars: List[Par[Option[A]]] = l map asyncF((a: A) => Some(a).filter(f))
    sequence(pars).map(_.flatten)
  }

  // original version
  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1) {
      unit(ints.headOption getOrElse 0)
    } else {
      val (l, r) = ints.splitAt(ints.length / 2)
      sum(l).map2(sum(r))(_ + _)
    }

  // Exercise 7.6 extension exercises

  // (1) Is there a more general version of the parallel summation function? Try using it to find the maximum
  //     value of an IndexedSeq in parallel

  // sum generalised in terms of arguments
  def fold1(ints: IndexedSeq[Int])(z: Int)(f: (Int, Int) => Int): Par[Int] =
    if (ints.size <= 1) {
      unit(ints.headOption getOrElse z)
    } else {
      val (l, r) = ints.splitAt(ints.length / 2)
      fold1(l)(z)(f).map2(fold1(r)(z)(f))(f)
    }

  def sumViaFold1(ints: IndexedSeq[Int]): Par[Int] =
    fold1(ints)(0)(_ + _)

  def maxValueViaFold1(ints: IndexedSeq[Int]): Par[Int] =
    fold1(ints)(Int.MinValue)((a, b) => if (a > b) a else b)

  // fold1 generalised in terms of types
  def fold2[A](items: IndexedSeq[A])(z: A)(f: (A, A) => A): Par[A] =
    if (items.size <= 1) {
      unit(items.headOption getOrElse z)
    } else {
      val (l, r) = items.splitAt(items.length / 2)
      fold2(l)(z)(f).map2(fold2(r)(z)(f))(f)
    }

  def sumViaFold2(ints: IndexedSeq[Int]): Par[Int] =
    fold2(ints)(0)(_ + _)

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
      wc2(l).map2(wc2(r))(_ + _)
    }
  }

  // Generalised on fn arguments
  def wc3(input: List[String])(f: String => Int)(g: (Int, Int) => Int): Par[Int] = {
    if (input.size <= 1) {
      unit(input.headOption.map(f).getOrElse(0))
    }
    else {
      val (l, r) = input.splitAt(input.length / 2)
      wc3(l)(f)(g).map2(wc3(r)(f)(g))(g)
    }
  }

  // Further generalised on type
  def wc4[A, B](input: List[A])(f: Option[A] => B)(g: (B, B) => B): Par[B] = {
    if (input.size <= 1)
      unit(f(input.headOption))
    else {
      val (l, r) = input.splitAt(input.length / 2)
      wc4(l)(f)(g).map2(wc4(r)(f)(g))(g)
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
    a.map2(b)((a, b) => f.curried(a)(b)).map2(c)((fcd, c) => fcd(c))
  }

  def map4[A, B, C, D, E](a: Par[A], b: Par[B], c: Par[C], d: Par[D])(f: (A, B, C, D) => E): Par[E] = {
    a.map2(b)((a, b) => f.curried(a)(b)).map2(c)((fcd, c) => fcd(c)).map2(d)((fde, d) => fde(d))
  }

  def map5[A, B, C, D, E, F](a: Par[A], b: Par[B], c: Par[C], d: Par[D], e: Par[E])(f: (A, B, C, D, E) => F): Par[F] = {
    a.map2(b)(
      (a, b) => f.curried(a)(b)).map2(c)((fcd, c) => fcd(c)).map2(d)((fde, d) => fde(d)).map2(e)((fef, e) => fef(e)
    )
  }

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

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
    choiceN(cond.map(b => if (b) 0 else 1))(List(t, f))

  // Exercise 7.12
  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
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
    join(p.map(f))
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

  //     Not 100% sure, but more threads required

  //   def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = {
  //    (es: ExecutorService) => {
  //      val af: Future[A] = a(es)
  //      val bf: Future[B] = b(es)
  //      val aVal = af.get
  //      val bVal = bf.get
  //      unit(f(aVal, bVal))(es)
  //    }
  //  }

  def map2Alt[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    flatMap(a)(a => flatMap(b)(b => unit(f(a, b))))

  def parSort3(parList: Par[List[Int]]): Par[List[Int]] = {
    map2Alt(parList, unit(()))((a, _) => a.sorted)
  }

  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  // infix versions of `map`, `map2`
  class ParOps[A](p: Par[A]) {
    def map[B](f: A => B): Par[B] = ParCompletableFuture.map(p)(f)

    def map2[B, C](b: Par[B])(f: (A, B) => C): Par[C] = ParCompletableFuture.map2(p, b)(f)

    def zip[B](b: Par[B]): Par[(A, B)] = p.map2(b)((_, _))
  }
}