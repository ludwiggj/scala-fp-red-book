package fpinscala.laziness

import scala.annotation.tailrec
import Stream.{cons, empty, unfoldTextbook}

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  // Exercise 5.1
  // The natural recursive solution
  // This solution will stack overflow for large streams, since it's not tail-recursive.
  def toListRecursiveTextbook: List[A] = this match {
    case Cons(h, t) => h() :: t().toListRecursiveTextbook
    case _ => List()
  }

  // My equivalent (more verbose)
  def toListRecursive: List[A] = {
    def loop(s: Stream[A], l: List[A]): List[A] = s match {
      case Empty => l
      case Cons(h, t) => h() +: loop(t(), l)
    }

    loop(this, List())
  }

  // Here is a tail-recursive implementation. At each step we cons onto the front of the `acc` list, which will
  // result in the reverse of the stream. Then at the end we reverse the result to get the correct order again.
  def toList: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), h() :: acc)
      case _ => acc
    }

    go(this, List()).reverse
  }

  // In order to avoid the `reverse` at the end, we can use a mutable list buffer and an explicit loop instead.
  // Note that the mutable list buffer never escapes our `toList` method, so this function is still pure.
  def toListFast: List[A] = {
    val buf = new collection.mutable.ListBuffer[A]

    @tailrec
    def go(s: Stream[A]): List[A] = s match {
      case Empty => buf.toList
      case Cons(h, t) => buf += h(); go(t())
    }

    go(this)
  }

  def toListViaFoldRight: List[A] =
    this.foldRight(List[A]())((h, t) => h +: t)

  // Exercise 5.2
  def take(n: Int): Stream[A] = {
    @tailrec
    def go(count: Int, s: Stream[A], acc: Stream[A]): Stream[A] = {
      if (count <= 0)
        Stream(acc.toListFast.reverse: _*)
      else s match {
        case Cons(h, t) => go(count - 1, t(), Cons(h, () => acc))
        case _ => Stream(acc.toListFast.reverse: _*)
      }
    }

    go(n, this, Empty)
  }

  def takeFast(n: Int): Stream[A] = {
    def reverseListBufferToStream(b: collection.mutable.ListBuffer[() => A]): Stream[A] = {
      @tailrec
      def go(l: List[() => A], s: Stream[A]): Stream[A] = l match {
        case h :: t => go(t, Cons(h, () => s))
        case _ => s
      }

      go(b.toList.reverse, Empty)
    }

    val buf = new collection.mutable.ListBuffer[() => A]

    @tailrec
    def go(count: Int, s: Stream[A]): Stream[A] = {
      if (count <= 0)
        reverseListBufferToStream(buf)
      else s match {
        case Cons(h, t) =>
          buf += h
          go(count - 1, t())
        case _ => reverseListBufferToStream(buf)
      }
    }

    go(n, this)
  }

  // take first checks if n is 0. In that case we need not look at the stream at all
  def takeTextbook(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def drop(n: Int): Stream[A] = {
    @tailrec
    def go(count: Int, s: Stream[A]): Stream[A] = {
      if (count <= 0)
        s
      else s match {
        case Cons(_, t) => go(count - 1, t())
        case _ => Empty
      }
    }

    go(n, this)
  }

  // Unlike take, `drop` is not incremental. That is, it doesn't generate the answer lazily.
  // It must traverse the first `n` elements of the stream eagerly.
  @tailrec
  final def dropTextbook(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().dropTextbook(n - 1)
    case _ => this
  }

  // Exercise 5.3
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) =>
      val a = h()
      if (p(a))
        cons(a, t().takeWhile(p))
      else
        empty

    case _ => empty
  }

  def takeWhileTextbook(f: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if f(h()) => cons(h(), t() takeWhile f)
    case _ => empty
  }

  def existsRecursive(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().existsRecursive(p)
    case _ => false
  }

  // The arrow => in front of the argument type B means that the function f takes its second argument by name
  // and may choose not to evaluate it
  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    // If f doesn't evaluate its second argument, the recursion never occurs
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  // Here b is the unevaluated recursive step that folds the tail of the stream.
  // If p(a) returns true, b will never be evaluated and the computation terminates early.
  def exists(p: A => Boolean): Boolean =
    foldRight(false)((h, t) => p(h) || t)

  // Exercise 5.4
  def forAllRecursive(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) && t().forAllRecursive(p)
    case _ => true
  }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((h, t) => p(h) && t)

  // Exercise 5.5
  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) =>
      if (p(h))
        cons(h, t)
      else
        empty
    )

  // Exercise 5.6
  def headOptionViaFoldRight: Option[A] =
    foldRight(None: Option[A])((h, _) => Some(h))

  // Exercise 5.7 map, filter, append, flatmap using foldRight.
  // Part of the exercise is writing your own function signatures
  def mapRecursive[B](f: A => B): Stream[B] = this match {
    case Cons(h, t) => cons(f(h()), t().mapRecursive(f))
    case _ => empty
  }

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h, acc) => cons(f(h), acc))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, acc) => if (p(h)) cons(h, acc) else acc)

  def appendElemIncorrect[B >: A](e: => B): Stream[B] = {
    foldRight(empty[B])((h, acc) => {
      if (acc == Empty) cons(h, Stream(e)) else cons(h, acc)
    })
  }

  def appendElem[B >: A](e: => B): Stream[B] =
    foldRight(Stream(e))((h, acc) => cons(h, acc))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, acc) => f(h).foldRight(acc)(cons(_, _)))

  // NOTE: Append signature should actually take a Stream!
  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h, acc) => cons(h, acc))

  def flatMapTextbook[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, acc) => f(h) append acc)

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  final def findAlternative(f: A => Boolean): Option[A] =
    this.filter(f).headOption

  // Exercise 5.13
  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfoldTextbook(this) {
      case Cons(h, t) => Some(f(h()), t())
      case _ => None
    }

  def takeViaUnfold(n: Int): Stream[A] =
    unfoldTextbook((this, n)) {
      case (a, n) if n > 0 => a match {
        case Cons(h, t) => Some(h(), (t(), n - 1))
        case _ => None
      }
      case _ => None
    }

  def takeViaUnfoldTextbook(n: Int): Stream[A] =
    unfoldTextbook((this, n)) {
      case (Cons(h, _), 1) => Some((h(), (empty, 0)))
      case (Cons(h, t), n) if n > 1 => Some((h(), (t(), n - 1)))
      case _ => None
    }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    unfoldTextbook(this) {
      case Cons(h, t) => if (p(h())) Some(h(), t()) else None
      case _ => None
    }

  def takeWhileViaUnfoldTextbook(f: A => Boolean): Stream[A] =
    unfoldTextbook(this) {
      case Cons(h, t) if f(h()) => Some((h(), t()))
      case _ => None
    }

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
    unfoldTextbook((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
      case _ => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfoldTextbook((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
      case (Cons(h1, t1), _) => Some((Some(h1()), None), (t1(), Empty))
      case (_, Cons(h2, t2)) => Some((None, Some(h2())), (Empty, t2()))
      case _ => None
    }

  // Textbook answers
  def zipTextbook[B](s2: Stream[B]): Stream[(A, B)] =
    zipWith(s2)((_, _))

  def zipAllTextbook[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    zipWithAllTextbook(s2)((_, _))

  def zipWithAllTextbook[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    unfoldTextbook((this, s2)) {
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) => Some(f(Some(h()), Option.empty[B]) -> (t(), empty[B]))
      case (Empty, Cons(h, t)) => Some(f(Option.empty[A], Some(h())) -> (empty[A] -> t()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
    }

  // Exercise 5.14
  def startsWith[A](s: Stream[A]): Boolean = {
    this.zipAll(s).forAll {
      case (Some(a1), Some(a2)) if a1 == a2 => true
      case (_, None) => true
      case _ => false
    }
  }

  /*
  `s startsWith s2` when corresponding elements of `s` and `s2` are all equal, until the point that `s2` is exhausted.
  If `s` is exhausted first, or we find an element that doesn't match, we terminate early. Using non-strictness,
  we can compose these three separate logical steps--the zipping, the termination when the second stream is exhausted,
  and the termination if a non-matching element is found or the first stream is exhausted.
  */
  def startsWithTextbook[A](s: Stream[A]): Boolean =
    zipAll(s).takeWhile(_._2.isDefined).forAll({
      case (h, h2) => h == h2
    })

  // Exercise 5.15
  def tails: Stream[Stream[A]] = {
    unfoldTextbook(cons(null.asInstanceOf[A], this)) {
      case Cons(_, t) => Some((t(), t()))
      case Empty => None
    }
  }

  // The last element of `tails` is always the empty `Stream`, so we handle this
  // as a special case, by appending it to the output.
  def tailsTextbook: Stream[Stream[A]] =
    unfoldTextbook(this) {
      case Empty => None
      case s => Some((s, s drop 1))
    } append Stream(empty)

  def hasSubsequence[B](s: Stream[B]): Boolean =
    tails.exists(_ startsWith s)

  // Exercise 5.16
  // This isn't efficient
  def scanRightInefficient[B](z: B)(f: (A, => B) => B): Stream[B] = {
    tailsTextbook.map(_.foldRight(z)(f))
  }

  // This is better
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight(Stream(z))((x, y) => cons(f(x, y.headOption.get), y))

  // The function can't be implemented using `unfold`, since `unfold` generates elements of the `Stream`
  // from left to right. It can be implemented using `foldRight` though.

  // The implementation is just a `foldRight` that keeps the accumulated value and the stream of intermediate results,
  // which we `cons` onto during each iteration. When writing folds, it's common to have more state in the fold than
  // is needed to compute the result. Here, we simply extract the accumulated list once finished.

  def scanRightTextbook[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, p0) => {
      // p0 is passed by-name and used in by-name args in f and cons. So use lazy val to ensure only one evaluation...
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, cons(b2, p1._2))
    })._2
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  // Recursive stream of 1's
  val ones: Stream[Int] = Stream.cons(1, ones)

  // Exercise 5.8
  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def constantTextbook[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  // Exercise 5.9
  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  // Exercise 5.10
  def fibs(a: Int = 0, b: Int = 1): Stream[Int] = Stream.cons(a, fibs(b, a + b))

  val fibsTextbook = {
    def go(f0: Int, f1: Int): Stream[Int] =
      cons(f0, go(f1, f0 + f1))

    go(0, 1)
  }

  // Exercise 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    def go(z: S, acc: Stream[A]): Stream[A] = {
      f(z) match {
        case Some((a, s)) => cons(a, unfold(s)(f))
        case None => acc
      }
    }

    go(z, Empty)
  }

  def unfoldTextbook[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((h, s)) => cons(h, unfold(s)(f))
      case None => empty
    }

  // Exercise 5.12
  val fibsViaUnfold: Stream[Int] =
    unfoldTextbook((0, 1)) { case (i, j) => Some(i, (j, i + j)) }

  val fibsViaUnfoldVerbose: Stream[Int] =
    unfoldTextbook((0, 1))(
      p => p match {
        case (i, j) => Some(i, (j, i + j))
      }
    )

  def fromViaUnfold(n: Int): Stream[Int] =
    unfoldTextbook(n)(n => Some(n, n + 1))

  def constantViaUnfold[A](a: A): Stream[A] =
    unfoldTextbook(a)(_ => Some(a, a))

  val onesViaUnfold: Stream[Int] =
    unfoldTextbook(1)(_ => Some(1, 1))
}

object StreamTests {

  def testHeadOption(): Unit = {
    println("headOption")
    println("==========")
    println()

    val stream = Cons(
      () => {
        println("Expensive Op!")
        "Bitcoin"
      },
      () => Empty
    )

    println(s"""Cons(() => { println("Expensive Op!"); 1}, () => Empty).headOption = ${stream.headOption}""")
    println(s"""Cons(() => { println("Expensive Op!"); 1}, () => Empty).headOption = ${stream.headOption}""")
    println()
  }

  def main(args: Array[String]): Unit = {
    testHeadOption()
    println(List(1, 2, 3).zip(List(4, 5, 6)))
    println(List(1, 2, 3).zip(List(4, 5)))
    println(List(1, 2, 3).zip(List()))
    println(List().zip(List(1, 2, 3)))
  }
}