package fpinscala.applicative

import fpinscala.UnitSpec
import fpinscala.applicative.FoldableFunctor.Iteration
import fpinscala.applicative.Traverse.{Tree, listTraverse, optionTraverse, treeTraverse}
import fpinscala.monoids.Monoid
import org.scalatest.Assertion

import scala.language.higherKinds

class IterationSpec extends UnitSpec {
  "foldMap" can "fold and map" in {
    assert(Iteration[Int](1, _ * 2, 5).foldMap(_.toString)(Monoid.stringMonoid) == "124816")
  }
}