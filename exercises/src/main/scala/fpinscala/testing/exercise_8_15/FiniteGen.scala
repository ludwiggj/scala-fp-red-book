package fpinscala.testing.exercise_8_15

import fpinscala.state.{RNG, State}
import fpinscala.testing.take2.{Gen, SGen}

// TODO - More work needed here!
case class FiniteGen[+A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))

  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    Gen(sample.flatMap(f(_).sample))
  }

//  def unsized: SGen[A] = {
//    SGen(_ => this)
//  }

//  def listOfN(size: Gen[Int]): Gen[List[A]] =
//    size flatMap (n => Gen.listOfN(n, this))
}
