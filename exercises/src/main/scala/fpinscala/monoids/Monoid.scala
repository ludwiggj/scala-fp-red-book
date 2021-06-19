package fpinscala.monoids

import fpinscala.monoids.Monoid.{dual, endoMonoid, foldMapV}
import fpinscala.parallelism.nonblocking.Par
import fpinscala.parallelism.nonblocking.Par._

import language.higherKinds

trait Monoid[A] {
  def op(a1: A, a2: A): A

  def zero: A
}

object Monoid {

  val stringMonoid: Monoid[String] = new Monoid[String] {
    def op(a1: String, a2: String): String = a1 + a2

    val zero: String = ""
  }

  def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2

    //noinspection TypeAnnotation
    val zero = Nil
  }

  // Exercise 10.1
  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2

    override def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2

    override def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

    override def zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

    override def zero: Boolean = true
  }

  // Exercise 10.2
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2

    override def zero: Option[A] = None
  }

  // We can get the dual of any monoid just by flipping the `op`.
  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)

    val zero: A = m.zero
  }

  // Exercise 10.3
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(a1: A => A, a2: A => A): A => A = a => a2(a1(a))

    override def zero: A => A = a => identity(a)
  }

  import fpinscala.testing.take2.{Gen, Prop}
  import fpinscala.testing.take2.Prop.forAll

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
    // Identity
    forAll(gen)(a => m.op(a, m.zero) == a && m.op(m.zero, a) == a) &&
      // Associativity
      forAll(for {
        x <- gen
        y <- gen
        z <- gen
      } yield (x, y, z))(p =>
        m.op(m.op(p._1, p._2), p._3) == m.op(p._1, m.op(p._2, p._3))
      )
  }

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  // Exercise 10.5
  // NOTE: This goes over the list twice
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    concatenate(as map f, m)

  // Use a single fold instead.
  def foldMapTextbook[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  // Exercise 10.6
  // foldRight using foldMap

  // Hint: Notice that the type of the function that is passed to `foldRight` is `(A, B) => B`,
  // which can be curried to `A => (B => B)`. This is a strong hint that we should use the
  // endofunction monoid `B => B` to implement `foldRight`.

  // The implementation of `foldLeft` is then just the dual.
  // Don't worry if these implementations are not very efficient.

  // The function type `(A, B) => B`, when curried, is `A => (B => B)`.
  // And of course, `B => B` is a monoid for any `B` (via function composition).
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(f.curried)(z)

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, dual(endoMonoid[B]))(a => b => f(b, a))(z)

  // Exercise 10.7
  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    as.length match {
      case 0 => m.zero
      case 1 => f(as(0))
      case _ =>
        val (left, right) = as.splitAt(as.length / 2)
        m.op(foldMapV(left, m)(f), foldMapV(right, m)(f))
    }
  }

  // Exercise 10.8, parallel version of foldMap
  def par[A](m: Monoid[A]): Monoid[Par[A]] = {
    new Monoid[Par[A]] {
      override def op(a1: Par[A], a2: Par[A]): Par[A] = Par.map2(a1, a2)(m.op)

      override def zero: Par[A] = Par.unit(m.zero)
    }
  }

  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    foldMapV(v, par(m))(a => Par.unit(f(a)))

  // Exercise 10.9
  // Hint: Try creating a data type which tracks the _interval_ of the values in a given segment, as well as whether an
  // 'unordered segment' has been found. When merging the values for two segments, think about how these two pieces of
  // information should be updated.

  // This implementation detects only ascending order,
  // but you can write a monoid that detects both ascending and descending
  // order if you like.
  def ordered(ints: IndexedSeq[Int]): Boolean = {
    // Our monoid tracks the minimum and maximum element seen so far
    // as well as whether the elements are so far ordered.
    val mon: Monoid[Option[(Int, Int, Boolean)]] = new Monoid[Option[(Int, Int, Boolean)]] {
      def op(o1: Option[(Int, Int, Boolean)], o2: Option[(Int, Int, Boolean)]): Option[(Int, Int, Boolean)] =
        (o1, o2) match {
          // The ranges should not overlap if the sequence is ordered.
          case (Some((x1, y1, p)), Some((x2, y2, q))) =>
            Some((x1 min x2, y1 max y2, p && q && y1 <= x2))
          case (x, None) => x
          case (None, x) => x
        }

      val zero: Option[(Int, Int, Boolean)] = None
    }
    // The empty sequence is ordered, and each element by itself is ordered.
    foldMapV(ints, mon)(i => Some((i, i, true))).forall(_._3)
  }

  sealed trait WC

  case class Stub(chars: String) extends WC

  case class Part(lStub: String, words: Int, rStub: String) extends WC

  // Exercise 10.10
  def countStub(stub: String): Int =
    if (stub.nonEmpty) 1 else 0

  def wcMonoid: Monoid[WC] = new Monoid[WC] {
    def splitString(input: String): Array[String] = {
      if (input.isEmpty) return Array.empty
      input.split("\\s") ++ (
        if (Character.isWhitespace(input.last))
          Array("")
        else
          Array.empty[String]
        )
    }

    def count(input: String): WC = {
      val words = splitString(input)
      words length match {
        case 0 => zero
        case 1 => Stub(words(0))
        case n => Part(words(0), n - 2, words(n - 1))
      }
    }

    override def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Stub(b), Stub(c)) =>
        count(b + c)

      case (Stub(b), Part(l, w, r)) =>
        Part(b + l, w, r)

      case (Part(l, w, r), Stub(b)) =>
        Part(l, w, r + b)

      case (Part(l1, w1, r1), Part(l2, w2, r2)) =>
        Part(l1, w1 + w2 + countStub(r1 + l2), r2)
    }

    override def zero: WC = Stub("")
  }

  // Note that this doesn't count the characters
  val wcMonoidTextbook: Monoid[WC] = new Monoid[WC] {
    // The empty result, where we haven't seen any characters yet.
    val zero: WC = Stub("")

    def op(a: WC, b: WC): WC = (a, b) match {
      case (Stub(c), Stub(d)) => Stub(c + d)
      case (Stub(c), Part(l, w, r)) => Part(c + l, w, r)
      case (Part(l, w, r), Stub(c)) => Part(l, w, r + c)
      case (Part(l1, w1, r1), Part(l2, w2, r2)) =>
        Part(l1, w1 + (if ((r1 + l2).isEmpty) 0 else 1) + w2, r2)
    }
  }

  // Exercise 10.11
  def count(s: String): Int = {
    val wc = wcMonoid

    def shouldBreakdown(input: String): Boolean = input.length > 20

    def go(s: String): WC = {
      val (left, right) = s.splitAt(s.length / 2)
      if (shouldBreakdown(left) || shouldBreakdown(right)) {
        wc.op(go(left), go(right))
      } else {
        wc.op(foldMap(List(left), wc)(Stub), foldMap(List(right), wc)(Stub))
      }
    }

    go(s) match {
      case Stub(chars) => countStub(chars)
      case Part(lStub, words, rStub) => countStub(lStub) + words + countStub(rStub)
    }
  }

  def countTextbook(s: String): Int = {
    def wc(c: Char): WC =
      if (c.isWhitespace)
        Part("", 0, "")
      else
        Stub(c.toString)

    def unstub(s: String): Int = s.length min 1

    foldMapV(s.toIndexedSeq, wcMonoid)(wc) match {
      case Stub(s) => unstub(s)
      case Part(l, w, r) => unstub(l) + w + unstub(r)
    }
  }

  // Exercise 10.16
  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    override def op(a1: (A, B), a2: (A, B)): (A, B) = (A.op(a1._1, a2._1), B.op(a1._2, a2._2))

    override def zero: (A, B) = (A.zero, B.zero)
  }

  // Monoid for merging key-value maps, as long as the value type is a monoid
  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
    override def op(a: Map[K, V], b: Map[K, V]): Map[K, V] =
      (a.keySet ++ b.keySet).foldLeft(zero) { (acc, k) =>
        acc.updated(k, V.op(a.getOrElse(k, V.zero), b.getOrElse(k, V.zero)))
      }

    override def zero: Map[K, V] = Map[K, V]()
  }

  // Exercise 10.17 - monoid instance for functions whose results are monoids
  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    override def op(a1: A => B, a2: A => B): A => B =
      a => B.op(a1(a), a2(a))

    override def zero: A => B = _ => B.zero
  }

  // Exercise 10.18 - use monoids to compute a "bag" from an IndexedSeq
  def bag[A](as: IndexedSeq[A]): Map[A, Int] = {
    val m: Monoid[Map[A, Int]] = mapMergeMonoid(intAddition)
    foldMapV(as, m)(a => Map(a -> 1))
  }
}

// Exercise 10.12
trait Foldable[F[_]] {
  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    foldMap(as)(f.curried)(endoMonoid[B])(z)

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    foldMap(as)(a => (b: B) => f(b, a))(dual(endoMonoid[B]))(z)

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  // Exercise 10.15
  def toList[A](fa: F[A]): List[A] = {
    foldRight(fa)(List.empty[A])(_ :: _)
  }
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    foldMapV(as, mb)(f)
}

// Just use scala Stream this time, as foldLeft not implemented and I'm too lazy :)
object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)
}

// Exercise 10.13
sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case Leaf(a) => f(a)
    case Branch(l, r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
  }

  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = as match {
    case Leaf(a) => f(a, z)
    case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
  }

  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = as match {
    case Leaf(a) => f(z, a)
    case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
  }
}

// Exercise 10.14
object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    as.fold(mb.zero)(f)

  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)

  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)
}

object OptionFoldableTextbook extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case None => mb.zero
    case Some(a) => f(a)
  }

  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B = as match {
    case None => z
    case Some(a) => f(z, a)
  }

  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B = as match {
    case None => z
    case Some(a) => f(a, z)
  }

  override def toList[A](fa: Option[A]): List[A] = fa match {
    case None => List()
    case Some(a) => List(a)
  }
}