package fpinscala.parallelism

object ParInitialSketch {

  class Par[A]

  object Par {
    def unit[A](a: => A): Par[A] = ???

    def get[A](a: Par[A]): A = ???

    def sum(ints: IndexedSeq[Int]): Int =
      if (ints.size <= 1) {
        ints.headOption getOrElse 0
      } else {
        val (l, r) = ints.splitAt(ints.length / 2)

        // Compute left half in parallel
        val sumL: Par[Int] = Par.unit(sum(l))

        // Compute right half in parallel
        val sumR: Par[Int] = Par.unit(sum(r))

        // Extract both result and sum them
        Par.get(sumL) + Par.get(sumR)
      }

    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = ???

    def sum2(ints: IndexedSeq[Int]): Par[Int] =
      if (ints.size <= 1) {
        unit(ints.headOption getOrElse 0)
      } else {
        val (l, r) = ints.splitAt(ints.length / 2)
        Par.map2(Par.sum2(l), Par.sum2(r))(_ + _)
      }

/*
Sum stack trace...
>>>
    sum2(IndexedSeq(1,2,3,4))
>>>
    map2(
      sum2(IndexedSeq(1,2)),
      sum2(IndexedSeq(3,4))
    )(_ + _)
>>>
    map2(
      map2(
        sum2(IndexedSeq(1)),
        sum2(IndexedSeq(2))
      )(_ + _),
      sum2(IndexedSeq(3,4))
    )(_ + _)
>>>
    map2(
      map2(
        unit(1),
        unit(2)
      )(_ + _),
      sum2(IndexedSeq(3,4))
    )(_ + _)
>>>
    map2(
      map2(
        unit(1),
        unit(2)
      )(_ + _),
      map2(
        sum2(IndexedSeq(3)),
        sum2(IndexedSeq(4))
      )(_ + _)
    )(_ + _)
etc...
*/
    def fork[A](a: => Par[A]): Par[A] = ???

    def sum3(ints: IndexedSeq[Int]): Par[Int] =
      if (ints.size <= 1) {
        Par.unit(ints.headOption getOrElse 0)
      } else {
        val (l, r) = ints.splitAt(ints.length / 2)
        Par.map2(Par.fork(sum3(l)), Par.fork(sum2(r)))(_ + _)
      }

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
  }
}