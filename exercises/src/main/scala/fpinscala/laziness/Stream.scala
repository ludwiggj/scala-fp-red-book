package fpinscala.laziness

import scala.annotation.tailrec
import Stream.{cons, empty}

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

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures
  def mapRecursive[B](f: A => B): Stream[B] = this match {
    case Cons(h, t) => cons(f(h()), t().mapRecursive(f))
    case _ => empty
  }

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h, t) => cons(f(h), t))

  def startsWith[B](s: Stream[B]): Boolean = ???

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
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

  val ones: Stream[Int] = Stream.cons(1, ones)

  def from(n: Int): Stream[Int] = ???

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = ???

  object Tests {
    def testHeadOption(): Unit = {
      println("headOption")
      println("==========")
      println()

      println(s"Stream().headOption = ${Stream().headOption}")
      println(s"Stream(1, 2, 3).headOption = ${Stream(1, 2, 3).headOption}")

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

    def test_Exercise_5_1(): Unit = {
      println("toList")
      println("======")
      println()

      def testToList[A](streamStr: String, s: Stream[A]): Unit = {
        println(s"$streamStr.toListRecursiveTextbook = ${s.toListRecursiveTextbook}")
        println(s"$streamStr.toListRecursive = ${s.toListRecursive}")
        println(s"$streamStr.toList = ${s.toList}")
        println(s"$streamStr.toListFast = ${s.toListFast}")
        println(s"$streamStr.toListViaFoldRight = ${s.toListViaFoldRight}")
        println()
      }

      testToList("Stream(1, 2, 3)", Stream(1, 2, 3))
      testToList("Stream()", Stream())
    }

    def test_Exercise_5_2(): Unit = {
      def testTake(count: Int): Unit = {
        val s = Stream(1, 2, 3, 4, 5)

        println(s"Stream(1, 2, 3, 4, 5).take($count).toList = ${s.take(count).toList}")
        println(s"Stream(1, 2, 3, 4, 5).takeFast($count).toList = ${s.takeFast(count).toList}")
        println(s"Stream(1, 2, 3, 4, 5).takeTextbook($count).toList = ${s.takeTextbook(count).toList}")
        println()
      }

      def testDrop(count: Int): Unit = {
        val s = Stream(1, 2, 3, 4, 5)
        println(s"Stream(1, 2, 3, 4, 5).drop($count).toList = ${s.drop(count).toList}")
        println(s"Stream(1, 2, 3, 4, 5).dropTextbook($count).toList = ${s.dropTextbook(count).toList}")
        println()
      }

      println("take")
      println("====")
      println()

      testTake(3)
      testTake(5)
      testTake(-1)
      testTake(6)

      println("drop")
      println("====")
      println()

      testDrop(3)
      testDrop(5)
      testDrop(-1)
      testDrop(6)
    }
  }

  def test_Exercise_5_3_5_5(): Unit = {
    println("takeWhile")
    println("=========")
    println()

    val s = Stream(1, 2, 3, 4, 5)

    println(s"Stream(1, 2, 3, 4, 5).takeWhile(_ < 3).toList = ${s.takeWhile(_ < 3).toList}")
    println(s"Stream(1, 2, 3, 4, 5).takeWhile(_ < 6).toList = ${s.takeWhile(_ < 6).toList}")
    println(s"Stream(1, 2, 3, 4, 5).takeWhile(_ > 2).toList = ${s.takeWhile(_ > 2).toList}")
    println(s"Stream(2, 4, 8, 0, 5).takeWhile(_ % 2 == 0).toList = ${Stream(2, 4, 8, 0, 5).takeWhile(_ % 2 == 0).toList}")
    println()

    println(s"Stream(1, 2, 3, 4, 5).takeWhileTextbook(_ < 3).toList = ${s.takeWhileTextbook(_ < 3).toList}")
    println(s"Stream(1, 2, 3, 4, 5).takeWhileTextbook(_ < 6).toList = ${s.takeWhileTextbook(_ < 6).toList}")
    println(s"Stream(1, 2, 3, 4, 5).takeWhileTextbook(_ > 2).toList = ${s.takeWhileTextbook(_ > 2).toList}")
    println(s"Stream(2, 4, 8, 0, 5).takeWhileTextbook(_ % 2 == 0).toList = ${Stream(2, 4, 8, 0, 5).takeWhileTextbook(_ % 2 == 0).toList}")
    println()

    println(s"Stream(1, 2, 3, 4, 5).takeWhileViaFoldRight(_ < 3).toList = ${s.takeWhileViaFoldRight(_ < 3).toList}")
    println(s"Stream(1, 2, 3, 4, 5).takeWhileViaFoldRight(_ < 6).toList = ${s.takeWhileViaFoldRight(_ < 6).toList}")
    println(s"Stream(1, 2, 3, 4, 5).takeWhileViaFoldRight(_ > 2).toList = ${s.takeWhileViaFoldRight(_ > 2).toList}")
    println(s"Stream(2, 4, 8, 0, 5).takeWhileViaFoldRight(_ % 2 == 0).toList = ${Stream(2, 4, 8, 0, 5).takeWhileViaFoldRight(_ % 2 == 0).toList}")
    println()
  }

  def testExists(): Unit = {
    def testExists(fnAsString: String, p: Int => Boolean): Unit = {
      val s = Stream(1, 2, 3, 4, 5)

      println(s"Stream(1, 2, 3, 4, 5).existsRecursive($fnAsString) = ${s.existsRecursive(p)}")
      println(s"Stream(1, 2, 3, 4, 5).exists($fnAsString) = ${s.exists(p)}")
      println()
    }

    println("exists")
    println("======")
    println()

    testExists("_ == 5", _ == 5)
    testExists("_ % 2 == 0", _ % 2 == 0)
    testExists("_ % 2 == 6", _ % 6 == 0)
  }

  def testExercise_5_4(): Unit = {
    println("forAll")
    println("======")
    println()

    def testForAll(fnAsString: String, p: Int => Boolean): Unit = {
      val s = Stream(1, 2, 3, 4, 5)

      println(s"Stream(1, 2, 3, 4, 5).forAllRecursive($fnAsString) = ${s.forAllRecursive(p)}")
      println(s"Stream(1, 2, 3, 4, 5).forAll($fnAsString) = ${s.forAll(p)}")
      println()
    }

    testForAll("_ < 5", _ < 5)
    testForAll("_ < 6", _ < 6)
    testForAll("_ < 0", _ < 0)
  }

  def testExercise_5_6(): Unit = {
    println("headOptionViaFoldRight")
    println("======================")
    println()

    println(s"Stream().headOptionViaFoldRight = ${Stream().headOptionViaFoldRight}")
    println(s"Stream(1, 2, 3).headOptionViaFoldRight = ${Stream(1, 2, 3).headOptionViaFoldRight}")
    println()
  }

  def testExercise_5_7(): Unit = {
    println("map")
    println("===")

    def testMap(count: Int): Unit = {
      val s = Stream(1, 2, 3, 4, 5)

      println(s"Stream(1, 2, 3, 4, 5).mapRecursive(_.toFloat).take($count) = ${s.mapRecursive(_.toFloat).take(count).toList}")
      println(s"Stream(1, 2, 3, 4, 5).map(_.toFloat).take($count) = ${s.map(_.toFloat).take(count).toList}")
      println()
    }

    testMap(5)
    testMap(3)
  }

  def main(args: Array[String]): Unit = {
    import Tests._

    testHeadOption()

    test_Exercise_5_1()

    test_Exercise_5_2()

    test_Exercise_5_3_5_5()

    testExists()

    testExercise_5_4()

    testExercise_5_6()

    testExercise_5_7()
  }
}