package fpinscala.iomonad

import fpinscala.iomonad.examples.Temperature

import scala.io.StdIn.readLine
import scala.language.postfixOps

object IO1 {
  // We need a way for our `IO` actions to yield a result of some
  // meaningful type. We do this by adding a type parameter to `IO`,
  // which now forms a `Monad`.

  sealed trait IO[A] {
    self =>
    def run: A

    def map[B](f: A => B): IO[B] = new IO[B] {
      def run: B = f(self.run)
    }

    def flatMap[B](f: A => IO[B]): IO[B] = new IO[B] {
      def run: B = f(self.run).run
    }
  }

  object IO extends Monad[IO] {
    // Above methods are all that is needed to fulfil the monad contract
    def unit[A](a: => A): IO[A] = new IO[A] {
      def run: A = a
    }

    def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa flatMap f

    def apply[A](a: => A): IO[A] = unit(a) // syntax for IO { .. }

    // Additional methods
    def ref[A](a: A): IO[IORef[A]] = IO {
      new IORef(a)
    }

    sealed class IORef[A](var value: A) {
      def set(a: A): IO[A] = IO {
        value = a;
        a
      }

      def get: IO[A] = IO {
        value
      }

      def modify(f: A => A): IO[A] = get flatMap (a => set(f(a)))
    }
  }

  // We can now express the example
  def ReadLine: IO[String] = IO {
    readLine
  }

  def PrintLine(msg: String): IO[Unit] = IO {
    println(msg)
  }

  import IO._ // import all the `IO` combinators that come from `Monad`

  // Larger example using various monadic combinators. Sample run:
  //
  // The Amazing Factorial REPL, v2.0
  // q - quit
  // <number> - compute the factorial of the given number
  // <anything else> - bomb with horrible error
  // 3
  // factorial: 6
  // 7
  // factorial: 5040
  // q
  def factorialRepl(): IO[Unit] = {
    val helpstring =
      """
        | The Amazing Factorial REPL, v2.0
        | q - quit
        | <number> - compute the factorial of the given number
        | <anything else> - bomb with horrible error
      """.trim.stripMargin

    def factorial(n: Int): IO[Int] = for {
      acc <- ref(1)
      _ <- foreachM(1 to n toStream)(i => acc.modify(_ * i).skip)
      result <- acc.get
    } yield result

    sequence_(
      IO {
        println(helpstring)
      },
      doWhile {
        IO {
          readLine
        }
      } {
        line =>
          val ok = line != "q"
          when(ok) {
            for {
              n <- factorial(line.toInt)
              _ <- IO {
                println("factorial: " + n)
              }
            } yield ()
          }
      }
    )
  }

  def echoInput: IO[Unit] =
    ReadLine.flatMap(PrintLine)

  def echoInputWithPrompt: IO[Unit] = {
    PrintLine("Type something and I will echo it back to you:")
      .flatMap(_ => ReadLine)
      .flatMap(PrintLine)
  }

  private def parseInt: IO[Unit] = {
    // Parses an `Int` by reading a line from the console.
    PrintLine("Enter an integer:")
      .flatMap(_ => ReadLine)
      .map(_.toInt)
      .flatMap(i => PrintLine(i.toString))
  }

  def parseTwoInts: IO[(Int, Int)] = {
    val readInt: IO[Int] = ReadLine.map(_.toInt)

    // Parses an `(Int,Int)` by reading two lines from the console.
    val readInts: IO[(Int, Int)] =
      PrintLine("Enter two integers:")
        .flatMap(_ => readInt ** readInt)
    readInts
  }

  def doWhileExample(): IO[Unit] =
    doWhile {
      IO {
        readLine
      }
    } {
      line =>
        PrintLine(line).flatMap(
          _ => IO.unit(line != "q")
        )
    }

  def foreverExample(): IO[Nothing] =
    forever(echoInput)

  def foldMExample(): IO[Int] =
    IO.foldM(Stream(1, 2, 3, 4))(1)((b, a) => {
      val product = b * a
      PrintLine(s"Passed values ($b, $a). Returning $product.").flatMap(_ => IO(product))
    })

  def foldM_Example(): IO[Unit] =
    IO.foldM_(Stream(1, 2, 3, 4))(1)((b, a) => {
      val product = b * a
      PrintLine(s"Passed values ($b, $a). Returning $product.").flatMap(_ => IO(product))
    })

  def foreachM_Example(): IO[Unit] =
    IO.foreachM(Stream(1, 2, 3, 4))(a => {
      PrintLine(s"Passed value $a")
    })

  def while_Example(b: Boolean): IO[Unit] = {
    IO.while_(IO(b))(PrintLine("A single iteration"))
  }

  def while_AnotherExample(): IO[Unit] = {
    def getEvenNumber: IO[Boolean] = {
      try {
        ReadLine.map(_.toInt % 2 == 0)
      } catch {
        case _: Exception => IO {
          false
        }
      }
    }

    IO.while_(getEvenNumber)(PrintLine("An even number"))
  }

  def main(args: Array[String]): Unit = {
    //    factorialRepl().run

    // Reads a line from the console and echoes it back.
    //    echoInput.run

    // Does the same but with a prompt
    //    echoInputWithPrompt.run

    //    parseInt.run

    //    parseTwoInts.flatMap(t => PrintLine(t.toString())).run

    //    replicateM_(3)(Temperature.converter3).run

    //    replicateM(3)(ReadLine).flatMap(l => PrintLine(l.mkString("|"))).run

    //    doWhileExample().run
    //    foreverExample().run
    //    println(foldMExample().run)
    //    println(foldM_Example().run)
    //    println(foreachM_Example().run)
    //    println(while_Example(false).run)
    //    println(while_Example(true).run) // Stack overflow!
    //    println(while_AnotherExample.run)
    //    forever(PrintLine("Still going...")).run // Stack overflow!

    // forever expanded is....

    // lazy val t: IO[Unit] = PrintLine("Still going...") flatMap (_ => t)
    // t.run

    // Stack trace shows that run is calling itself over and over

    // Exception in thread "main" java.lang.StackOverflowError
    //	at sun.nio.cs.UTF_8$Encoder.encodeLoop(UTF_8.java:691)
    //	at java.nio.charset.CharsetEncoder.encode(CharsetEncoder.java:579)
    //	at sun.nio.cs.StreamEncoder.implWrite(StreamEncoder.java:271)
    //	at sun.nio.cs.StreamEncoder.write(StreamEncoder.java:125)
    //	at java.io.OutputStreamWriter.write(OutputStreamWriter.java:207)
    //	at java.io.BufferedWriter.flushBuffer(BufferedWriter.java:129)
    //	at java.io.PrintStream.write(PrintStream.java:526)
    //	at java.io.PrintStream.print(PrintStream.java:669)
    //	at java.io.PrintStream.println(PrintStream.java:823)
    //	at scala.Console$.println(Console.scala:267)
    //	at scala.Predef$.println(Predef.scala:393)
    //	at fpinscala.iomonad.IO1$.$anonfun$PrintLine$1(IO1.scala:60)
    //	at scala.runtime.java8.JFunction0$mcV$sp.apply(JFunction0$mcV$sp.java:12)
    //	at fpinscala.iomonad.IO1$IO$$anon$3.run(IO1.scala:29)
    //	at fpinscala.iomonad.IO1$IO$$anon$2.run(IO1.scala:22)
    //	at fpinscala.iomonad.IO1$IO$$anon$2.run(IO1.scala:22)
    //	at fpinscala.iomonad.IO1$IO$$anon$2.run(IO1.scala:22)
    //	at fpinscala.iomonad.IO1$IO$$anon$2.run(IO1.scala:22)

    // problem is in definition of flatMap

    // def flatMap[B](f: A => IO[B]): IO[B] = new IO[B] {
    //   def run: B = f(self.run).run
    // }

    // Method creates new IO object whose run definition calls run again before calling f
    // Will keep building up nested run calls on the stack and eventually overflow it

    // In our example:

    // lazy val t: IO[Unit] = PrintLine("Still going...") flatMap (_ => t)

    // Result of first call to flatMap
    new IO[Unit] {
      def run: Unit = {
        val selfRun: Unit = PrintLine("Still going...").run
        // This method terminates, but only because recursive call commented out (see next line)
        val f: Unit => IO[Unit] = _ => PrintLine("Still going...") // flatMap (_ => t) [next flatMap term]
        f(selfRun).run // This will create next recursion
      }
    }.run

    // Effectively above code recurses down the stack, running a PrintLine at each level before recursing downwards
    // This is due to lazy recursive definition - next level down is only evaluated when required, which creates
    // the side effect at each level. But call never returns so the stack gets deeper until it overflows
  }
}