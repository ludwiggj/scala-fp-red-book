package fpinscala.parallelism.nonblocking

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}
import Par._

// Note the key idea is that fork and map2 should not block (unlike the previous implementation of Par)
object Par {

  sealed trait Future[+A] {
    // Function A => Unit is a callback
    private[parallelism] def apply(cb: A => Unit): Unit
  }

  type Par[+A] = ExecutorService => Future[A]

  // run now returns an A rather than a Future[A]
  // it will block on waiting for the result
  def run[A](es: ExecutorService)(p: Par[A]): A = {
    // A mutable, threadsafe reference, to use for storing the result
    val ref = new AtomicReference[A]

    // A latch which, when decremented, implies that `ref` has the result
    val latch = new CountDownLatch(1)

    // Asynchronously set the result, and decrement the latch
    // p(es) { a => ref.set(a); latch.countDown() }

    // More explicit version
    def cb(a: A): Unit = {
      ref.set(a)
      latch.countDown()
    }

    p(es).apply(cb)

    // Block until the `latch.countDown` is invoked asynchronously
    latch.await()

    // Once we've passed the latch, we know `ref` has been set, and return its value
    ref.get
  }

  def unit[A](a: A): Par[A] = {
    // Note that the executor service isn't needed
    _ =>
      new Future[A] {
        def apply(cb: A => Unit): Unit =
          cb(a)
      }
  }

  /** A non-strict version of `unit` */
  def delay[A](a: => A): Par[A] =
    _ => new Future[A] {
      def apply(cb: A => Unit): Unit =
        cb(a)
    }

  // This is where we introduce the actual parallelism
  // NOTE: This version of fork does not block
  def fork[A](a: => Par[A]): Par[A] =
    es => new Future[A] {
      def apply(cb: A => Unit): Unit = {
        // eval forks off evaluation of a, and returns immediately
        // The callback will be invoked asynchronously on another thread
        eval(es)(a(es)(cb))
      }
    }

  /**
   * Helper function, for evaluating an action
   * asynchronously, using the given `ExecutorService`.
   */
  def eval(es: ExecutorService)(r: => Unit): Unit = {
    // Submit returns a Future[Unit], which is discarded
    // The ExecutorService provides methods that can produce a Future
    // for tracking progress of one or more asynchronous tasks.
    // call is the method that is called asynchronously
    es.submit(new Callable[Unit] {
      def call: Unit = r
    })
  }

  /**
   * Helper function for constructing `Par` values out of calls to non-blocking continuation-passing-style APIs.
   * This will come in handy in Chapter 13.
   */
  def async[A](f: (A => Unit) => Unit): Par[A] = _ => new Future[A] {
    def apply(k: A => Unit): Unit = f(k)
  }

  // Map2 is implemented using actors
  // NOTE: This version of fork does not block
  def map2[A, B, C](p: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] =
    es => new Future[C] {
      def apply(cb: C => Unit): Unit = {
        // Two mutable vars are used to store the results
        var ar: Option[A] = None
        var br: Option[B] = None

        // Combiner is an actor that awaits both results, combines them with f
        // and passes the results to cb
        val combiner = Actor[Either[A, B]](es) {
          // A result arrives
          case Left(a) =>
            // If B result already arrived, call f with both results and pass resulting C to the callback
            if (br.isDefined) {
              println(s"($a, ${br.get})")
              eval(es)(cb(f(a, br.get)))
            }
            // otherwise store result and await B result
            else ar = Some(a)
          // B result arrives
          case Right(b) =>
            // If A result already arrived, call f with both results and pass resulting C to the callback
            if (ar.isDefined) {
              println(s"(${ar.get}, $b)")
              eval(es)(cb(f(ar.get, b)))
            }
            // otherwise store result and await A result
            else br = Some(b)
        }

        // Pass the combiner actor as a continuation to both side
        // A side is wrapped in a left, B in a right, so actor can pattern match on the message
        p(es)(a => combiner ! Left(a))
        p2(es)(b => combiner ! Right(b))
      }
    }

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  // specialized version of `map`
  def map[A, B](p: Par[A])(f: A => B): Par[B] =
    es => new Future[B] {
      def apply(cb: B => Unit): Unit =
        p(es)(a => eval(es) {
          cb(f(a))
        })
    }

  def lazyUnit[A](a: => A): Par[A] =
    fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  def sequenceRight[A](as: List[Par[A]]): Par[List[A]] =
    as match {
      case Nil => unit(Nil)
      case h :: t => h.map2(fork(sequence(t)))(_ :: _)
    }

  def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
    if (as.isEmpty) unit(Vector())
    else if (as.length == 1) as.head.map(a => Vector(a))
    else {
      val (l, r) = as.splitAt(as.length / 2)
      sequenceBalanced(l).map2(sequenceBalanced(r))(_ ++ _)
    }
  }

  def sequence[A](as: List[Par[A]]): Par[List[A]] =
    sequenceBalanced(as.toIndexedSeq).map(_.toList)

  // Reimplementing selected exercises from Par class
  def parSort(parList: Par[List[Int]]): Par[List[Int]] = parList.map(_.sorted)

  // Exercise 7.6
  def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars: List[Par[List[A]]] =
      l map asyncF((a: A) => if (f(a)) List(a) else List())
    // convenience method on `List` for concatenating a list of lists
    sequence(pars).map(_.flatten)
  }

  // exercise answers

  /*
   * We can implement `choice` as a new primitive.
   *
   * `p(es)(result => ...)` for some `ExecutorService`, `es`, and
   * some `Par`, `p`, is the idiom for running `p`, and registering
   * a callback to be invoked when its result is available. The
   * result will be bound to `result` in the function passed to
   * `p(es)`.
   *
   * If you find this code difficult to follow, you may want to
   * write down the type of each subexpression and follow the types
   * through the implementation. What is the type of `p(es)`? What
   * about `t(es)`? What about `t(es)(cb)`?
   */
  def choice[A](p: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es => new Future[A] {
      def apply(cb: A => Unit): Unit =
        p(es) { b =>
          if (b) eval(es) {
            t(es)(cb)
          }
          else eval(es) {
            f(es)(cb)
          }
        }
    }

  def choiceN[A](p: Par[Int])(ps: List[Par[A]]): Par[A] =
    es => new Future[A] {
      def apply(cb: A => Unit): Unit =
        p(es) { i =>
          eval(es) {
            ps(i % ps.size)(es)(cb)
          }
        }
    }

  def choiceViaChoiceN[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] =
    choiceN(a.map(b => if (b) 0 else 1))(List(ifTrue, ifFalse))

  def choiceMap[K, V](p: Par[K])(ps: Map[K, Par[V]]): Par[V] =
    es => new Future[V] {
      def apply(cb: V => Unit): Unit =
        p(es) { k =>
          eval(es) {
            ps(k)(es)(cb)
          }
        }
    }

  // see `Nonblocking.scala` answers file. This function is usually called something else!
  def chooser[A, B](p: Par[A])(f: A => Par[B]): Par[B] =
    es => new Future[B] {
      def apply(cb: B => Unit): Unit =
        p(es) { a =>
          eval(es) {
            f(a)(es)(cb)
          }
        }
    }

  def flatMap[A, B](p: Par[A])(f: A => Par[B]): Par[B] =
    chooser(p)(f)

  def choiceViaChooser[A](p: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    chooser(p)(a => if (a) t else f)

  def choiceNViaChooser[A](p: Par[Int])(choices: List[Par[A]]): Par[A] =
    chooser(p)(choice => choices(choice % choices.size))

  def join[A](p: Par[Par[A]]): Par[A] =
    es => new Future[A] {
      def apply(cb: A => Unit): Unit = {
        cb(run(es)(run(es)(p)))
      }
    }

  def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] =
    flatMap(a)(identity)

  def flatMapViaJoin[A, B](p: Par[A])(f: A => Par[B]): Par[B] =
    join(p.map(f))

  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  // infix versions of `map`, `map2`
  class ParOps[A](p: Par[A]) {
    def map[B](f: A => B): Par[B] = Par.map(p)(f)

    def map2[B, C](b: Par[B])(f: (A, B) => C): Par[C] = Par.map2(p, b)(f)

    def zip[B](b: Par[B]): Par[(A, B)] = p.map2(b)((_, _))
  }

}

// Exercise 7.10 - Non-blocking representation doesn't handle errors
//                 If computation throws an exception then exception
//                 is swallowed and latch never counts down. Can you
//                 fit that?

// Hint: Try adding a second continuation argument to `Future.apply`, which takes an error handler.
object Run2 {

  import fpinscala.errorhandling.Either

  def run[A](es: ExecutorService)(p: Par[Either[Exception, A]]): Either[Exception, A] = {
    // A mutable, threadsafe reference, to use for storing the result
    val ref = new AtomicReference[Either[Exception, A]]

    // A latch which, when decremented, implies that `ref` has the result
    val latch = new CountDownLatch(1)

    // Asynchronously set the result, and decrement the latch
    p(es) {
      a => {
        ref.set(a)
        latch.countDown()
      }
    }

    // Block until the `latch.countDown` is invoked asynchronously
    latch.await()

    // Once we've passed the latch, we know `ref` has been set, and return its value
    ref.get
  }
}