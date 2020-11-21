package fpinscala.datastructures

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  val tree1: Tree[Int] = Leaf(1)

  val tree2: Tree[Int] = Branch(
    left = Branch(
      left = Leaf(5),
      right = Branch(
        left = Leaf(7),
        right = Leaf(5)
      )
    ),
    right = Leaf(4)
  )

  val tree3: Tree[Int] = Branch(
    left = Leaf(1),
    right = Leaf(1)
  )

  val tree4: Tree[Int] = Branch(
    left = Leaf(9),
    right = Branch(
      left = Leaf(20),
      right = Leaf(5)
    )
  )

  object Exercise_3_25 {
    def size[A](t: Tree[A]): Int = t match {
      case Leaf(_) => 1
      case Branch(left, right) => 1 + size(left) + size(right)
    }
  }

  object Exercise_3_26 {
    def maximum(t: Tree[Int]): Int = {
      def go(t: Tree[Int], currentMax: Int): Int = t match {
        case Leaf(v) => v max currentMax
        case Branch(l, r) => go(l, currentMax) max go(r, currentMax)
      }

      go(t, Int.MinValue)
    }

    def maximumTextbook(t: Tree[Int]): Int = t match {
      case Leaf(n) => n
      case Branch(l, r) => maximumTextbook(l) max maximumTextbook(r)
    }
  }

  object Exercise_3_27 {
    def depth[A](t: Tree[A]): Int = t match {
      case Leaf(_) => 0
      case Branch(l, r) => (1 + depth(l)) max (1 + depth(r))
    }

    def depth2(t: Tree[Int]): Int = t match {
      case Leaf(_) => 0
      case Branch(l, r) => 1 + (depth2(l) max depth2(r))
    }
  }

  object Exercise_3_28 {
    def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
      case Leaf(v) => Leaf(f(v))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }
  }

  object Exercise_3_29 {

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
      as match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
      }
    }

    // Like `foldRight` for lists, `fold` receives a "handler" for each of the data constructors of the type,
    // and recursively accumulates some value using these handlers. As with `foldRight`,
    // `fold(t)(Leaf(_))(Branch(_,_)) == t`, and we can use this function to implement just about any recursive
    // function that would otherwise be defined by pattern matching.
    def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
      case Leaf(v) => f(v)
      case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }

    def size[A](t: Tree[A]): Int =
      fold(t)(_ => 1) { case (l, r) => 1 + l + r }

    def sizeTextbook[A](t: Tree[A]): Int =
      fold(t)(a => 1)(1 + _ + _)

    def maximum[A](t: Tree[Int]): Int =
      fold(t)(a => a) { case (l, r) => l max r }

    def maximumTextbook(t: Tree[Int]): Int =
      fold(t)(a => a)(_ max _)

    def depth[A](t: Tree[A]): Int =
      fold(t)(_ => 0) { case (l, r) => 1 + (l max r) }

    def depthTextbook[A](t: Tree[A]): Int =
      fold(t)(a => 0)((d1, d2) => 1 + (d1 max d2))

    def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
      fold[A, Tree[B]](t)(a => Leaf(f(a))) { case (l, r) => Branch(l, r) }

    // Note the type annotation required on the expression `Leaf(f(a))`. Without this annotation, we get an error
    // like this:
    //
    // type mismatch;
    //   found   : fpinscala.datastructures.Branch[B]
    //   required: fpinscala.datastructures.Leaf[B]
    //   fold(t)(a => Leaf(f(a)))(Branch(_,_))
    //                                  ^
    // This error is an unfortunate consequence of Scala using subtyping to encode algebraic data types.
    // Without the annotation, the result type of the fold gets inferred as `Leaf[B]` and it is then expected
    // that the second argument to `fold` will return `Leaf[B]`, which it doesn't (it returns `Branch[B]`).
    // Really, we'd prefer Scala to infer `Tree[B]` as the result type in both cases. When working with algebraic
    // data types in Scala, it's somewhat common to define helper functions that simply call the corresponding
    // data constructors but give the less specific result type:

    def mapTextbook[A, B](t: Tree[A])(f: A => B): Tree[B] =
      fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))

    def leaf[A](a: A): Tree[A] = Leaf(a)
    def branch[A](l: Tree[A], r: Tree[A]): Tree[A] = Branch(l, r)

    def mapTextbook2[A, B](t: Tree[A])(f: A => B): Tree[B] =
      fold(t)(a => leaf(f(a)))(branch)
  }
}

object TreeTestExercises {

  import Tree._

  def testTreeFn(treeFnName: String, treeFn: Tree[Int] => Int): Unit = {
    println(s"$treeFnName of $tree1 = ${treeFn(tree1)}")
    println(s"$treeFnName of $tree2 = ${treeFn(tree2)}")
    println(s"$treeFnName of $tree3 = ${treeFn(tree3)}")
    println(s"$treeFnName of $tree4 = ${treeFn(tree4)}")
    println()
  }

  def testExercise3_25(): Unit = {
    import Exercise_3_25._

    println("Exercise 3.25, size")
    println("===================")

    testTreeFn("size", size)
  }

  def testExercise3_26(): Unit = {
    import Exercise_3_26._

    println("Exercise 3.26, maximum")
    println("======================")

    testTreeFn("maximum", maximum)
    testTreeFn("maximumTextbook", maximumTextbook)
  }

  def testExercise3_27(): Unit = {
    import Exercise_3_27._

    println("Exercise 3.27, depth")
    println("====================")

    testTreeFn("depth", depth)
    testTreeFn("depth2", depth2)
  }

  def testExercise3_28(): Unit = {
    import Exercise_3_28._

    println("Exercise 3.28, map")
    println("==================")

    println(s"map of $tree1(_ * 5) = ${map(tree1)(_ * 5)}")
    println(s"map of $tree2(_ * 3) = ${map(tree2)(_ * 3)}")
    println(s"map of $tree3(_ + 1) = ${map(tree3)(_ + 1)}")
    println(s"map of $tree4(x => A * x) = ${map(tree4)(x => "A" * x)}")
    println()
  }

  def testExercise3_29(): Unit = {
    import Exercise_3_29._

    println("Exercise 3.29, fold")
    println("===================")

    println("size")
    println("----")
    testTreeFn("size", size)
    testTreeFn("sizeTextbook", sizeTextbook)

    println("maximum")
    println("-------")
    testTreeFn("maximum", maximum)
    testTreeFn("maximumTextbook", maximumTextbook)

    println("depth")
    println("-----")
    testTreeFn("depth", depth)
    testTreeFn("depthTextbook", depthTextbook)

    println("map")
    println("---")
    println(s"map of $tree1(_ * 5) = ${map(tree1)(_ * 5)}")
    println(s"map of $tree2(_ * 3) = ${map(tree2)(_ * 3)}")
    println(s"map of $tree3(_ + 1) = ${map(tree3)(_ + 1)}")
    println(s"map of $tree4(x => A * x) = ${map(tree4)(x => "A" * x)}")
    println()
    println(s"mapTextbook of $tree1(_ * 5) = ${mapTextbook(tree1)(_ * 5)}")
    println(s"mapTextbook of $tree2(_ * 3) = ${mapTextbook(tree2)(_ * 3)}")
    println(s"mapTextbook of $tree3(_ + 1) = ${mapTextbook(tree3)(_ + 1)}")
    println(s"mapTextbook of $tree4(x => A * x) = ${mapTextbook(tree4)(x => "A" * x)}")
    println()
    println(s"mapTextbook2 of $tree1(_ * 5) = ${mapTextbook2(tree1)(_ * 5)}")
    println(s"mapTextbook2 of $tree2(_ * 3) = ${mapTextbook2(tree2)(_ * 3)}")
    println(s"mapTextbook2 of $tree3(_ + 1) = ${mapTextbook2(tree3)(_ + 1)}")
    println(s"mapTextbook2 of $tree4(x => A * x) = ${mapTextbook2(tree4)(x => "A" * x)}")
    println()
  }

  def main(args: Array[String]): Unit = {
    testExercise3_25()
    testExercise3_26()
    testExercise3_27()
    testExercise3_28()
    testExercise3_29()
  }
}