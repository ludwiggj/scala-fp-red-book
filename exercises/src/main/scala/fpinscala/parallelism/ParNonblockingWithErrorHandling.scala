package fpinscala.parallelism

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}
import scala.language.implicitConversions
import scala.util.{Either, Left, Right}

// Note the key idea is that fork and map2 should not block (unlike the previous implementation of Par)
object ParNonblockingWithErrorHandling {

  sealed trait Future[+A] {
    // Function A => Unit is a callback
    private[parallelism] def apply(cb: A => Unit, eh: Exception => Unit): Unit
  }

  type Par[+A] = ExecutorService => Future[A]

  object Par {
    // TODO Should work, see https://stackoverflow.com/questions/38699210/scala-overloaded-method-value-cannot-be-applied

    //    def run[A](es: ExecutorService)(p: Par[A]): Either[Exception, A] = {
    //      run(es)(p) {ex: Exception => println(s"Exception thrown: ${ex.getMessage}")}
    //    }

    def run[A](es: ExecutorService)(p: Par[A]): Either[Exception, A] = {
      runWithExceptionHandler(es)(p) { ex: Exception => println(s"Exception thrown: ${ex.getMessage}"); ex }
    }

    def runWithExceptionHandler[A](es: ExecutorService)
                                  (p: Par[A])
                                  (customEh: Exception => Exception): Either[Exception, A] = {
      // A mutable, threadsafe reference, to use for storing the result
      val ref = new AtomicReference[Either[Exception, A]]

      // A latch which, when decremented, implies that `ref` has the result
      val latch = new CountDownLatch(1)

      // More explicit version
      def cb(a: A): Unit = {
        ref.set(Right(a))
        latch.countDown()
      }

      def eh(ex: Exception): Unit = {
        ref.set(Left(customEh(ex)))
        latch.countDown()
      }

      p(es).apply(cb, eh)

      // Block until the `latch.countDown` is invoked asynchronously
      latch.await()

      // Once we've passed the latch, we know `ref` has been set, and return its value
      ref.get
    }

    def lazyUnit[A](a: => A): Par[A] = {
      // Note that the executor service isn't needed
      _ =>
        new Future[A] {
          def apply(cb: A => Unit, eh: Exception => Unit): Unit = {
            try {
              cb(a)
            } catch {
              case e: Exception => eh(e)
            }
          }
        }
    }

    def parLazyUnit[A](a: => A): Par[A] = {
      fork(lazyUnit(a))
    }

    // This is where we introduce the actual parallelism
    // NOTE: This version of fork does not block
    def fork[A](a: => Par[A]): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit, eh: Exception => Unit): Unit = {
          // eval forks off evaluation of a, and returns immediately
          // The callback will be invoked asynchronously on another thread
          eval(es)(a(es)(cb, eh))
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

    // specialized version of `map`
    def map[A, B](p: Par[A])(f: A => B): Par[B] =
      es => new Future[B] {
        def apply(cb: B => Unit, eh: Exception => Unit): Unit =
          p(es)(a => eval(es)(cb(f(a))), eh)
      }

    // Map2 is implemented using actors
    // NOTE: This version of fork does not block
    def map2[A, B, C](p: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] =
      es => new Future[C] {
        def apply(cb: C => Unit, eh: Exception => Unit): Unit = {
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
          p(es)(a => combiner ! Left(a), eh)
          p2(es)(b => combiner ! Right(b), eh)
        }
      }

    def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
      if (as.isEmpty) lazyUnit(Vector())
      else if (as.length == 1) as.head.map(a => Vector(a))
      else {
        val (l, r) = as.splitAt(as.length / 2)
        sequenceBalanced(l).map2(sequenceBalanced(r))(_ ++ _)
      }
    }

    def sequence[A](as: List[Par[A]]): Par[List[A]] =
      sequenceBalanced(as.toIndexedSeq).map(_.toList)

    def sequenceRight[A](as: List[Par[A]]): Par[List[A]] =
      as match {
        case Nil => lazyUnit(Nil)
        case h :: t => h.map2(fork(sequence(t)))(_ :: _)
      }

    def parSort(parList: Par[List[Int]]): Par[List[Int]] = parList.map(_.sorted)

    def parSort(l: => List[Int]): Par[List[Int]] = lazyUnit(l).map(_.sorted)

    // TODO - This call fails if ps contains an exception, as it is
    //        evaluated when map is called on it
    //        See below for a version that works
    def parMap[A, B](ps: => List[A])(f: A => B): Par[List[B]] = fork {
      try {
        val fbs: List[Par[B]] = ps.map(asyncF(f))
        sequence(fbs)
      }
      catch {
        case _: Exception => lazyUnit(List())
      }
    }

    // This version fixes the issue (similar to parSort above)
    def parMapFixed[A, B](ps: => List[A])(f: A => B): Par[List[B]] = fork {
      lazyUnit(ps.map(f))
    }

    def asyncF[A, B](f: A => B): A => Par[B] =
      a => lazyUnit(f(a))

    // TODO - This call fails if l contains an exception, as it is
    //        evaluated when map is called on it
    //        See below for a version that works
    def parFilter[A](l: => List[A])(f: A => Boolean): Par[List[A]] = {
      try {
        val pars: List[Par[List[A]]] =
          l map asyncF((a: A) => if (f(a)) List(a) else List())
        // convenience method on `List` for concatenating a list of lists
        sequence(pars).map(_.flatten)
      } catch {
        case e: Exception => lazyUnit(List())
      }
    }

    // This version fixes the issue (similar to parSort above)
    def parFilterFixed[A](l: => List[A])(f: A => Boolean): Par[List[A]] = {
      lazyUnit(l flatMap ((a: A) => if (f(a)) List(a) else List()))
    }

    def choice[A](p: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit, eh: Exception => Unit): Unit =
          p(es)(b =>
            if (b) eval(es) {
              t(es)(cb, eh)
            }
            else eval(es) {
              f(es)(cb, eh)
            }
            , eh)
      }

    def choiceN[A](p: Par[Int])(ps: List[Par[A]]): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit, eh: Exception => Unit): Unit =
          p(es)(i =>
            eval(es) {
              ps(i % ps.size)(es)(cb, eh)
            }
            , eh)
      }

    def choiceViaChoiceN[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] =
      choiceN(a.map(b => if (b) 0 else 1))(List(ifTrue, ifFalse))

    def choiceMap[K, V](p: Par[K])(ps: Map[K, Par[V]]): Par[V] =
      es => new Future[V] {
        def apply(cb: V => Unit, eh: Exception => Unit): Unit =
          p(es)(k =>
            eval(es) {
              ps(k)(es)(cb, eh)
            }
            , eh)
      }

    // see `Nonblocking.scala` answers file. This function is usually called something else!
    def chooser[A, B](p: Par[A])(f: A => Par[B]): Par[B] =
      es => new Future[B] {
        def apply(cb: B => Unit, eh: Exception => Unit): Unit =
          p(es)(a =>
            eval(es) {
              f(a)(es)(cb, eh)
            }
            , eh)
      }

    def flatMap[A, B](p: Par[A])(f: A => Par[B]): Par[B] =
      chooser(p)(f)

    def choiceViaChooser[A](p: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      chooser(p)(a => if (a) t else f)

    def choiceNViaChooser[A](p: Par[Int])(choices: List[Par[A]]): Par[A] =
      chooser(p)(choice => choices(choice % choices.size))

    def join[A](p: Par[Par[A]]): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit, eh: Exception => Unit): Unit = {
          run(es)(p) match {
            case Right(value) =>
              run(es)(value) match {
                case Right(value) =>
                  cb(value)
                case _ =>
              }
            case _ =>
          }
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
    }

  }

}