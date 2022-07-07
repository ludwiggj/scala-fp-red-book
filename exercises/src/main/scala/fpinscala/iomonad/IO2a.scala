package fpinscala.iomonad

object IO2a {

  // The previous IO representation overflows the stack for some programs.
  // The problem is that `run` call itself recursively, which means that
  // an infinite or long running IO computation will have a chain of regular
  // calls to `run`, eventually overflowing the stack.

  // The general solution is to make the `IO` type into a data type that we
  // interpret using a tail recursive loop, using pattern matching.

  sealed trait IO[A] {
    // NOTE: We don't define run here, so that case classes don't have to implement them

    // We do not interpret the `flatMap` here, just return it as a value
    def flatMap[B](f: A => IO[B]): IO[B] =
      FlatMap(this, f)

    def map[B](f: A => B): IO[B] =
      flatMap(f andThen (Return(_)))
  }

  // A pure computation that immediately returns an A without any further steps.
  // When run sees this constructor, it knows the computation has finished.

  // Return represents an IO action that has finished - we want to return the
  // value without any further steps.
  case class Return[A](a: A) extends IO[A]

  // A suspension of the computation where resume is a function that takes
  // no arguments, but has some effect and yields a result

  // Suspend means that we want to execute some effect to produce a result.
  case class Suspend[A](resume: () => A) extends IO[A]

  // A composition of two steps. Reifies flatMap as a data constructor rather
  // than a function. When run sees this, it should first process the
  // subcomputation sub and then continue with k once sub produces a result

  // Flatmap lets us extend/continue an existing computation by using the
  // result of the first computation to produce a second computation

  // It interprets the subcomputation sub and then calls the continuation k on the result
  case class FlatMap[A, B](sub: IO[A], k: A => IO[B]) extends IO[B]

  // Notice that none of these operations DO anything
  object IO extends Monad[IO] {
    def unit[A](a: => A): IO[A] = Return(a)

    def flatMap[A,B](a: IO[A])(f: A => IO[B]): IO[B] = a flatMap f

    def suspend[A](a: => IO[A]): IO[A] =
      Suspend(() => ()).flatMap { _ => a }
  }

  // PrintLine in terms of IO
  def printLine(s: String): IO[Unit] =
    Suspend(() => Return(println(s)))

  def printTwoLines(l1: String, l2: String): IO[Unit] =
    Suspend(() => Return(println(l1))).flatMap(_ => Suspend(() => Return(println(l2))))

  // TODO Why do these variations behave the way that they do?
  def printTwoLines2(l1: String, l2: String): IO[Unit] =
//    Return(println(l1)).flatMap(_ => Return(println(l2)))                 // Only prints second string in loop
//    Return(println(l1)).flatMap(_ => Suspend(() => Return(println(l2))))  // Only prints second string in loop
    Suspend(() => Return(println(l1))).flatMap(_ => Return(println(l2)))

  // There is only one sensible way to implement this as a tail-recursive function.
  @annotation.tailrec
  def run[A](io: IO[A]): A = io match {
    // Return[A](a: A) extends IO[A]
    // Just return the result
    case Return(a) => a

    // Suspend[A](resume: () => A) extends IO[A]
    // Execute the effect to produce the result
    case Suspend(r) => r()

    // FlatMap[A, B](x: IO[A], f: A => IO[B]) extends IO[B]
    // Extend the computation (execute a subroutine)
    case FlatMap(x, f) => x match {
      // Overall case: FlatMap(Return(a), f)
      // Evaluate subcomputation: Return(a) reduces to a (end of computation)
      // Call f on it to continue the computation
      case Return(a) => run(f(a))             // Ignore the type signature errors, this compiles ok

      // Overall case: FlatMap(Suspend(r), f)
      // Evaluate subcomputation: Call r to produce the effect
      // Call f on it to continue the computation
      case Suspend(r) => run(f(r()))          // Ignore the type signature errors, this compiles ok

      // Overall case: FlatMap(FlatMap(y, g), f)
      // FlatMap[A, B](y: IO[A], g: A => IO[B]) extends IO[B]
      case FlatMap(y, g) =>
        // The one tricky case is left-nested flatMaps, as in `((y flatMap g) flatMap f)`,
        // which we reassociate to the right as `y flatMap (a => g(a) flatMap f)
        // This also fulfils the contract to evaluate subcomputation and then call f on it
        run(y flatMap (a => g(a) flatMap f))  // Ignore the type signature errors, this compiles ok
    }
  }

  def main(args: Array[String]): Unit = {
    // run(IO.forever(printLine("Still going...")))

    // This becomes:
    // lazy val t: IO[Unit] = printLine("Still going...") flatMap (_ => t)
    // run(t)

    // Suspend(() => Return(println("Still going..."))) flatMap (_ => t)

    // def flatMap[B](f: A => IO[B]): IO[B] = FlatMap(this, f)

    // FlatMap(Suspend(() => Return(println("Still going..."))),
    //         _ => FlatMap(Suspend(() => Return(println("Still going..."))),
    //              _ => FlatMap(...)
    //         ))

    // Printing two lines
//    run(IO.forever(printTwoLines("Red lorry", "Yellow Lorry")))
//    run(printTwoLines2("Red lorry", "Yellow Lorry"))
//    run(IO.forever(printTwoLines2("Red lorry", "Yellow Lorry")))

    // Trampolining - a general solution to stack overflow (Page 239)
    val f = (x: Int) => x
    val g = List.fill(100000)(f).foldLeft(f)(_ compose _)
    // g(42) // Stack overflow!

    // Solve using our IO monad
    val ff: Int => IO[Int] = (x: Int) => Return(x)

    val gg: Int => IO[Int] = List.fill(100000)(ff).foldLeft(ff) {
      (a: Function1[Int, IO[Int]],
       b: Function1[Int, IO[Int]]) => x => Suspend(() => ()).flatMap { _ => a(x).flatMap(b) }
    }

    println(run(gg(42)))

    // Helper function to make this nicer
    def suspend[A](a: => IO[A]): IO[A] = Suspend(() => ()).flatMap { _ => a }

    val ggg = List.fill(100000)(ff).foldLeft(ff) {
      (a, b) => x => suspend { a(x).flatMap(b) }
    }

    println(run(ggg(52)))

    val gggg = List.fill(100000)(ff).foldLeft(ff) {
      (a, b) => x => IO.suspend { a(x).flatMap(b) }
    }

    println(run(gggg(62)))

    val actions: Stream[IO[Unit]] = Stream.fill(100000)(printLine("Still going..."))
    val composite: IO[Unit] = actions.foldLeft(IO.unit(())) { (acc, a) => acc flatMap { _ => a } }
    // run(composite)
  }
}