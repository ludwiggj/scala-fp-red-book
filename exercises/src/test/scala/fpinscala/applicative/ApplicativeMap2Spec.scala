package fpinscala.applicative

import fpinscala.UnitSpec

import scala.language.higherKinds

class ApplicativeMap2Spec extends UnitSpec {
  val F: ApplicativeMap2[Option] = new ApplicativeMap2[Option] {
    override def unit[A](a: => A): Option[A] = Some(a)

    override def map2[A, B, C](fa: Option[A], fb: Option[B])(f: (A, B) => C): Option[C] = (fa, fb) match {
      case (Some(a), Some(b)) => Some(f(a, b))
      case _ => None
    }
  }

  // Exercise 12.1
  "sequence" can "sequence elements" in {
    assert(F.sequence(List(None, None)).isEmpty)
    assert(F.sequence(List(None, Some(5), Some(12), None)).isEmpty)
    assert(F.sequence(List(Some(5), Some(12))).contains(List(5, 12)))
  }

  "replicateM" can "create a list of n elements" in {
    assert(F.replicateM(2, Some(3)).contains(List(3, 3)))
    assert(F.replicateM(2, None).isEmpty)
  }

  "product" can "combine functors" in {
    assert(F.product(Some(5), Some(List("hello", "fox"))).contains((5, List("hello", "fox"))))
    assert(F.product(None, Some(List("hello", "fox"))).isEmpty)
  }
}