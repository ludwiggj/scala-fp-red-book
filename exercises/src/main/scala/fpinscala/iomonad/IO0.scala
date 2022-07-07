package fpinscala.iomonad

import fpinscala.monoids.Monoid

object IO0 {
  // Our first attempt at data type for representing computations that
  // may perform I/O. Has a simple 'interpreter' baked in--the `run`
  // function, which just returns `Unit`.

  trait IO extends Monoid[IO] {
    self =>
    def run: Unit

    def ++(io: IO): IO = new IO {
      def run: Unit = {
        self.run; io.run
      }
    }

    def empty: IO = new IO {
      def run = ()
    }

    // Methods required to implement monoid
    override def op(a1: IO, a2: IO): IO = a1.++(a2)

    override def zero: IO = empty
  }

  def PrintLine(msg: String): IO0.IO =
    new IO0.IO {
      def run: Unit = println(msg)
    }

  // The API of this `IO` type isn't very useful.  Not many operations
  // (it is only a monoid - see above), and not many laws to help with
  // reasoning. It is completely opaque. Also cannot represent input
  // effects, like reading from console, for instance. See temperature
  // example for details.
}
