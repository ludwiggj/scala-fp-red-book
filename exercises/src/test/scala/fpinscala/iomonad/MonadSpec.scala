package fpinscala.iomonad

import fpinscala.UnitSpec

class MonadSpec extends UnitSpec {

  private val sut = new Monad[Option] {
    override def unit[A](a: => A): Option[A] = Option(a)
    override def flatMap[A, B](a: Option[A])(f: A => Option[B]): Option[B] = a flatMap f
  }

  "foldM" can "fold the stream with the function f, combining the effects" in {
    val s = Stream(1, 2, 3, 4)
    assert(sut.foldM(s)(1)((b, a) => Option(b * a)).contains(24))
  }

  "as" can "produce new result, ignoring existing one" in {
    assert(sut.as(Option(5))("ta da!") == Option("ta da!"))
  }

  "skip" can "return unit, ignoring existing effect" in {
    assert(sut.skip(Option(5)) == Option(()))
  }

  "replicateM" can "replicate Option instances" in {
    assert(sut.replicateM(5)(Some(3)).contains(List(3, 3, 3, 3, 3)))
  }

  // def when[A](b: Boolean)(fa: => F[A]): F[Boolean] =
  //    if (b) as(fa)(true) else unit(false)
  //    if (b) map(fa)(_ => true) else unit(false)
  "when" can "map the effect to true when condition is true" in {
    assert(sut.when(b = true)(Some(9)).contains(true))
    assert(sut.when(b = true)(None).isEmpty)
  }

  it can "discard the effect and return false when condition is false" in {
    assert(sut.when(b = false)(Some(9)).contains(false))
    assert(sut.when(b = false)(None).contains(false))
  }

  "seq" can "sequence functions" in {
    def f(i: Int): Option[String] = Option(i.toString)
    def g(s: String): Option[Int] = Option(s.toInt * 2)

    assert(sut.seq(f)(g).apply(2).contains(4))
  }
}







