package fpinscala.datastructures

import scala.annotation.tailrec

// `List` data type, parameterized on a type, `A`
sealed trait List[+A]

// A `List` data constructor representing the empty list
case object Nil extends List[Nothing]

// Another data constructor, representing nonempty lists.
// Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.
case class Cons[+A](head: A, tail: List[A]) extends List[A]

// `List` companion object. Contains functions for creating and working with lists.
object List {
  // A function that uses pattern matching to add up a list of integers
  def sum(l: List[Int]): Int = l match {
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // Time taken is proportional to length of first parameter, a1
  def appendList[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, appendList(t, a2))
    }

  // Fold right:

  // def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B

  // Applies a binary operator to all elements of this list and a start value, going right to left.
  // Returns the result of inserting op between consecutive elements of this list (x1, ..., xn), going right to left
  // with the start value z on the right:

  // f(x_1, f(x_2, ... f(x_n, z)))
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  }

  def sum2(ns: List[Int]): Int =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]): Double =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`

  // Test foldLeft and foldRight with long lists
  def constructList(len: Int): List[Int] = {
    var l: List[Int] = Nil
    for (_ <- scala.List.range(0, len)) {
      l = Cons(1, l)
    }
    l
  }

  object Exercise_3_2 {
    // Although we could return `Nil` when the input list is empty, we choose to throw an exception instead. This is
    // a somewhat subjective choice. In our experience, taking the tail of an empty list is often a bug, and silently
    // returning a value just means this bug will be discovered later, further from the place where it was introduced.
    def tail[A](l: List[A]): List[A] = l match {
      case Nil => sys.error("tail of empty list")
      case Cons(_, t) => t
    }
  }

  object Exercise_3_3 {
    // If a function body consists solely of a match expression, we'll often put the match on the same line as the
    // function signature, rather than introducing another level of nesting.
    def setHead[A](l: List[A], h: A): List[A] = l match {
      case Nil => sys.error("setHead on empty list")
      case Cons(_, t) => Cons(h, t)
    }
  }

  object Exercise_3_4 {
    // Again, it's somewhat subjective whether to throw an exception when asked to drop more elements than the list
    // contains. The usual default for `drop` is not to throw an exception, since it's typically used in cases where this
    // is not indicative of a programming error. If you pay attention to how you use `drop`, it's often in cases where the
    // length of the input list is unknown, and the number of elements to be dropped is being computed from something else.
    // If `drop` threw an exception, we'd have to first compute or check the length and only drop up to that many elements.
    @scala.annotation.tailrec
    def drop[A](l: List[A], n: Int): List[A] = {
      if (n <= 0) l else l match {
        case Nil => Nil
        case Cons(_, t) => drop(t, n - 1)
      }
    }
  }

  object Exercise_3_5 {
    @scala.annotation.tailrec
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
      case Nil => l
      case Cons(h, t) => if (f(h)) dropWhile(t, f) else l
    }

    // The pattern guard only matches a `Cons` whose head satisfies our predicate, `f`
    def dropWhileTextbook[A](l: List[A], f: A => Boolean): List[A] =
      l match {
        case Cons(h, t) if f(h) => dropWhile(t, f)
        case _ => l
      }

    // Change arguments into two lists assists type inference
    def dropWhile2[A](l: List[A])(f: A => Boolean): List[A] = l match {
      case Nil => l
      case Cons(h, t) => if (f(h)) dropWhile(t, f) else l
    }
  }

  object Exercise_3_6 {
    // We're copying the entire list up until the last element. Besides being inefficient, the natural recursive
    // solution will use a stack frame for each element of the list, which can lead to stack overflows for
    // large lists
    def init[A](l: List[A]): List[A] = l match {
      case Nil => sys.error("init on empty list")
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }

    // With lists, it's common to use a temporary, mutable buffer internal to the function (with lazy lists or streams,
    // we don't normally do this). So long as the buffer is allocated internal to the function, the mutation is not
    // observable and RT is preserved.
    def init2[A](l: List[A]): List[A] = {
      import collection.mutable.ListBuffer
      val buf = new ListBuffer[A]

      @annotation.tailrec
      def go(cur: List[A]): List[A] = cur match {
        case Nil => sys.error("init of empty list")
        case Cons(_, Nil) => List(buf.toList: _*)
        case Cons(h, t) => buf += h; go(t)
      }

      go(l)
    }

    // Another common convention is to accumulate the output list in reverse order, then reverse it at the end, which
    // doesn't require even local mutation. The following example does not include the final reversal of the list.
    def initReverse[A](l: List[A]): List[A] = {
      @scala.annotation.tailrec
      def go(cur: List[A], acc: List[A]): List[A] = cur match {
        case Nil => sys.error("init of empty list")
        case Cons(_, Nil) => acc
        case Cons(h, t) => go(t, Cons(h, acc))
      }

      go(l, Nil)
    }
  }

  object Exercise_3_7 {
    // This is not possible! The reason is because before we ever call our function, `f`, we evaluate its argument,
    // which in the case of `foldRight` means traversing the list all the way to the end. We need non-strict evaluation
    // to support early termination
    def productWithShortCircuit(ns: List[Double]): Nothing = ???
  }

  object Exercise_3_9 {
    def lengthRecursive[A](l: List[A]): Int = {
      @tailrec
      def go(l: List[A], len: Int = 0): Int = l match {
        case Cons(_, t) => go(t, len + 1)
        case _ => len
      }

      go(l)
    }

    // Remember foldRight definition:

    // def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    //    as match {
    //      case Nil => z
    //      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    //    }
    //  }

    // Example expansion for sum:

    // foldRight(Cons(3, Cons(2, Cons(1, Nil))), 0)((x,y) => x + y)
    // 3 + foldRight(Cons(2, Cons(1, Nil)), 0)((x,y) => x + y)
    // 3 + 2 + foldRight(Cons(1, Nil), 0)((x,y) => x + y)
    // 3 + 2 + 1 + foldRight(Nil, 0)((x,y) => x + y)
    // 3 + 2 + 1 + 0

    def length[A](l: List[A]): Int =
      foldRight(l, 0)((_, acc) => acc + 1)

    // Example expansion:

    // foldRight(Cons(3, Cons(2, Cons(1, Nil))), 0)((x,y) => y + 1)
    // foldRight(Cons(2, Cons(1, Nil)), 0)((x,y) => y + 1) + 1
    // foldRight(Cons(1, Nil), 0)((x,y) => y + 1) + 1 + 1
    // foldRight(Nil, 0)((x,y) => y + 1) + 1 + 1 + 1
    // 0 + 1 + 1 + 1

    // OR

    // f (3, f (2, f ( 1, z ) ) )
    // f (3, f (2, f ( 1, 0 ) ) )
    // f (3, f (2, 1 ) )
    // f (3, 2 )
    // 3

    // Uses current element of list instead of the accumulator
    def lengthIncorrect1[A](l: List[A]): Int =
      foldRight(l, 0)((a, _) => a.asInstanceOf[Int] + 1)

    // Example expansion:

    // foldRight(Cons(3, Cons(2, Cons(1, Nil))), 0)((x,y) => x.asInstanceOf[Int] + 1)

    // NOTE: Not sure how to expand in traditional terms, as f(x, y) does not include y

    // OR

    // f (3, f (2, f ( 1, z ) ) )
    // f (3, f (2, f ( 1, 0 ) ) )
    // f (3, f (2, 2 ) )
    // f (3, 3 )
    // 4

    // Returns a constant
    def lengthIncorrect2[A](l: List[A]): Int =
      foldRight(l, 0)((_, _) => 1)

    // Example expansion:

    // foldRight(Cons(3, Cons(2, Cons(1, Nil))), 0)((x,y) => 1)

    // NOTE: Not sure how to expand in traditional terms, as f(x, y) does not include y

    // OR

    // f (3, f (2, f ( 1, z ) ) )
    // f (3, f (2, f ( 1, 0 ) ) )
    // f (3, f (2, 1 ) )
    // f (3, 1 )
    // 1
  }

  object Exercise_3_10 {
    def foldLeftNotTailRecursive[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
      case Nil => z
      case Cons(h, t) => f(foldLeftNotTailRecursive(t, z)(f), h)
    }

    // e.g.
    // foldLeftNotTailRecursive(l, 0)((acc, _) => acc + 1)
    //
    // foldLeftNotTailRecursive(Cons(1, Cons(2, Cons(3, Nil))), 0)
    // f(foldLeftNotTailRecursive(Cons(2, Cons(3, Nil)), 0), 1)
    // f(f(foldLeftNotTailRecursive(Cons(3, Nil), 0), 1), 2)
    // f(f(f(foldLeftNotTailRecursive(Nil, 0), 1), 2),3)
    // f(f(f(0, 1), 2),3)
    // f(f(1, 2),3)
    // f(2,3)
    // 3

    // It's common practice to annotate functions you expect to be tail-recursive with the `tailrec` annotation.
    // If the function is not tail-recursive, it will yield a compile error, rather than silently compiling the
    // code and resulting in greater stack space usage at runtime.

    // def foldLeft[A, B](l: List[A], z: B)(f: (B, A) ⇒ B): B

    // Applies a binary operator to a start value and all elements of this sequence (x1, ..., xn), going left to right.
    // Returns the result of inserting op between consecutive elements of this sequence, going left to right with the
    // start value z on the left:

    // f(f(f(z, x_1), x_2), ..., x_n)
    @tailrec
    def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

    // This version of foldRight is tail recursive, but it doesn't work!
    // See explanation below, and exercise 10 tests...
    // Also see https://stackoverflow.com/questions/4085118/why-foldright-and-reduceright-are-not-tail-recursive
    @tailrec
    def foldRightTailRec[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
      as match {
        case Nil => z
        case Cons(h, t) => foldRightTailRec(t, f(h, z))(f)
      }
    }

    // Fold right should be:
    // f (x_1, f (x_2, ... f (x_n, z ) ) )

    // Above definition:

    // foldRightTailRec[A, B]((x_1, x_2, ... x_n), z: B)(f)

    // => foldRightTailRec((x_2, ... x_n), f(x_1, z))(f)

    // => foldRightTailRec(( ... x_n), f(x_2, f(x_1, z)))(f)

    // => f(x_n, ... f(x_2, f(x_1, z)))

    // Therefore it's the opposite way around!
  }

  object Exercise_3_11 {

    import Exercise_3_10.foldLeft

    def sum(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

    def product(l: List[Int]): Int = foldLeft(l, 1)(_ * _)

    def length[A](l: List[A]): Int = foldLeft(l, 0)((acc, _) => acc + 1)
  }

  object Exercise_3_12 {

    import Exercise_3_10.foldLeft

    def reverseRecursive[A](l: List[A]): List[A] = {
      @scala.annotation.tailrec
      def go(cur: List[A], acc: List[A]): List[A] = cur match {
        case Cons(h, t) => go(t, Cons(h, acc))
        case _ => acc
      }

      go(l, Nil)
    }

    def reverse[A](l: List[A]): List[A] = {
      foldLeft(l, List[A]())((acc, h) => Cons(h, acc))
    }
  }

  object Exercise_3_13 {
    import Exercise_3_10.foldLeft
    import Exercise_3_12.reverse

    def foldLeftViaFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
      foldRight(reverse(l), z)((a: A, b: B) => f(b, a))
    }

    // foldLeftViaFoldRight((x_1, x_2, ... x_n), z)(f)

    // => foldRight((x_n, ..., x_2, x_1), z)(f') // List elements reversed, arguments to f reversed

    // FR: f(x_1, f(x_2, ... f(x_n, z)))

    // => f(f(f(z, x_1), x_2), ... x_n) // This is fold left!

    // FL: f(f(f(z, x_1), x_2), ..., x_n)

    // The implementation of `foldRight` in terms of `reverse` and `foldLeft` is a common trick for avoiding stack
    // overflows when implementing a strict `foldRight` function.
    def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
      foldLeft(reverse(l), z)((b: B, a: A) => f(a, b))
    }

    // foldRightViaLeftFold((x_1, x_2, ... x_n), z)(f)

    // => foldLeft((x_n, ..., x_2, x_1), z)(f') // List elements reversed, arguments to f reversed

    // FL: f(f(f(z, x_1), x_2), ..., x_n)

    // => f(x1, f(x_2, f(..., f(x_n, z)))) // This is fold right!

    // FR: f(x_1, f(x_2, ... f(x_n, z)))


    // There are other implementations that don't use reverse. They build up a chain of functions which, when called,
    // results in the operations being performed with the correct associativity.

    //EXPLANATION 1:

    // We are calling `foldRight` with the `B` type being instantiated to `B => B`, then calling the built up function
    // with the `z` argument.

    // Try expanding the definitions by substituting equals for equals using a simple example, like
    // `foldLeft(List(1,2,3), 0)(_ + _)` if this isn't clear.

    // Note these implementations are more of theoretical interest - they aren't stack-safe and won't work
    // for large lists.

    // Definitions of foldLeft/foldRight:

    //  def  foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B
    //  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B

    def foldLeftViaFoldRight2[A,B](l: List[A], z: B)(f: (B,A) => B): B =
      foldRight(l, (b:B) => b)((a, g) => b => g(f(b,a)))(z)

    def foldRightViaFoldLeft2[A,B](l: List[A], z: B)(f: (A,B) => B): B =
      foldLeft(l, (b:B) => b)((g,a) => b => g(f(a,b)))(z)

    // Here is the same function with much more description
    def foldLeftViaFoldRight3[A,B](as: List[A], outerIdent: B)(combiner: (B, A) => B): B = {
      // foldLeft processes items in the reverse order from foldRight.  It's
      // cheating to use reverse() here because that's implemented in terms of
      // foldLeft!  Instead, wrap each operation in a simple identity function to
      // delay evaluation until later and stack (nest) the functions so that the
      // order of application can be reversed.  We'll call the type of this
      // particular identity/delay function BtoB so we aren't writing B => B
      // everywhere:
      type BtoB = B => B

      // Here we declare a simple instance of BtoB according to the above
      // description.  This function will be the identity value for the inner
      // foldRight.
      def innerIdent:BtoB = (b:B) => b

      // For each item in the 'as' list (the 'a' parameter below), make a new
      // delay function which will use the combiner function (passed in above)
      // when it is evaluated later.  Each new function becomes the input to the
      // previous function (delayFunc).
      //
      //                        This much is just the type signature
      //                  ,-------^-------.
      def combinerDelayer:(A, BtoB) => BtoB =
        (a: A, delayFunc: BtoB) => (b:B) => delayFunc(combiner(b, a))
      // `----------v---------'    `----------------v---------------'
      //         Parameters                 The returned function

      // Pass the original list 'as', plus the simple identity function and the
      // new combinerDelayer to foldRight.  This will create the functions for
      // delayed evaluation with an combiner inside each one, but will not
      // apply any of those functions.
      def go:BtoB = foldRight(as, innerIdent)(combinerDelayer)

      // This forces all the evaluations to take place
      go(outerIdent)
    }

    // Here is a sample run for further illustration purposes

    // foldLeftViaFoldRight3(List(1,2,3), Nil:List[Int])((b:B, a:A) => Cons(a, b))

    // Becomes:

    // foldRight(List(1,2,3), innerIdent)(combinerDelayer)(Nil)

    // Remember expansion of foldRight:

    // f(x_1, f(x_2, ... f(x_n, z)))

    // Expansion:

    // Term: f(x_3, z)
    // => combinerDelayer(3, innerIdent)
    // => (b: B) => innerIdent(combiner(b, 3)
    // def delay3(b:B) = innerIdent(combiner(b,  3))

    // Term: f(x_2, delay3)
    // => combinerDelayer(2, delay3)
    // => (b: B) => delay3(combiner(b, 2)
    // def delay2(b:B) = delay3(combiner(b, 2))

    // Similarly term: f(x_1, delay2)
    // => (b: B) => delay2(combiner(b, 1)
    // def delay1(b:B) = delay2(combiner(b,  1))

    // Expanding delay1:

    // def delay1(b:B) = delay2(combiner(b,  1))

    // where:
    // def delay2(b:B) = delay3(combiner(b, 2))

    // so:
    // def delay1(b:B) = delay3(combiner(combiner(b,  1), 2))

    // where:
    // def delay3(b:B) = innerIdent(combiner(b,  3))

    // so:
    // def delay1(b:B) = innerIdent(combiner(combiner(combiner(b,  1), 2),  3))

    // def delay1(b:B) = combiner(combiner(combiner(b,  1), 2),  3)

    // This is called with outerIndent (Nil)

    // = combiner(combiner(combiner(Nil,  1), 2),  3)

    // = combiner(combiner(Cons(1,  Nil), 2),  3)

    // = combiner(Cons(2, Cons(1,  Nil)),  3)

    // = Cons(3, Cons(2, Cons(1,  Nil)))

    // EXPLANATION 2:

    // See http://stackoverflow.com/questions/17136794/foldleft-using-foldright-in-scala

    // def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B,A) => B): B =
    //   foldRight(l, (b:B) => b)((a,g) => b => g(f(b,a)))(z)

    //   The trick is that during the right fold operation, we don't build the final value of type B.
    //   Instead, we build a function from B to B. The fold step takes a value of type a: A and a
    //   function g: B => B and produces a new function
    //
    //   (b => g(f(b,a))): B => B.
    //
    //   This function can be expressed as a composition of g with f(_, a):
    //
    //   l.foldRight(identity _)((a,g) => g compose (b => f(b,a)))(z);
    //
    //   We can view the process as follows: For each element a of l we take the partial application
    //   b => f(b,a), which is a function B => B. Then, we compose all these functions in such a way
    //   that the function corresponding to the rightmost element (with which we start the traversal)
    //   is at far left in the composition chain. Finally, we apply the big composed function on z.
    //   This results in a sequence of operations that starts with the leftmost element (which is at
    //   far right in the composition chain) and finishes with the right most one.

    //   As an example, let's examine how this definition works on a two-element list. First, we'll
    //   rewrite the function as

    //   def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B,A) => B): B = {
    //     def h(a: A, g: B => B): (B => B) = g compose ((x: B) => f(x,a));
    //     l.foldRight(identity[B] _)(h _)(z);
    //   }

    // Now let's compute what happens when we pass it List(1,2)

    //   def foldLeftViaFoldRight[A,B](List(1, 2), z: B)(f: (B,A) => B): B = {
    //     def h(a: A, g: B => B): (B => B) = g compose ((x: B) => f(x,a));
    //     l.foldRight(identity[B] _)(h _)(z);
    //   }

    // List(1,2).foldRight(identity[B] _)(h _)(z)
    // by the definition of the right fold, which is op(x_1, op(x_2, ... op(x_n, z)))

    // h(1, h(2, identity([B])))

    // expand the inner `h`
    // h(1, identity[B] compose ((x: B) => f(x, 2)))
    // h(1, ((x: B) => f(x, 2)))

    // expand the other `h`
    // ((x: B) => f(x, 2)) compose ((x: B) => f(x, 1))

    // by the definition of function composition
    // (y: B) => f(f(y, 1), 2)

    // Applying this function to z yields

    // f(f(z, 1), 2)

    // as required, because definition of foldLeft is op(op(op(z, x_1), x_2), ..., x_n)
  }

  object Exercise_3_14 {

    import Exercise_3_10.foldLeft
    import Exercise_3_12.reverse

    def appendElem[A](l: List[A], elem: A): List[A] = {
      foldRight(l, List(elem))((h, t) => Cons(h, t))
    }

    def appendElem2[A](l: List[A], elem: A): List[A] = {
      foldLeft(reverse(l), List(elem))((t, h) => Cons(h, t))
    }

    def appendList[A](l: List[A], r: List[A]): List[A] = {
      foldRight(l, r)(Cons(_, _))
    }
  }

  object Exercise_3_15 {
    /*
    def foldRight[B](z: B)(op: (A, B) ⇒ B): B

    Returns the result of inserting op between consecutive elements of this list, going right to left
    with the start value z on the right:

    op(x_1, op(x_2, ... op(x_n, z)))

    where x1, ..., xn are the elements of this list.

    def foldLeft[B](z: B)(f: (B, A) ⇒ B): B

    Returns the result of inserting op between consecutive elements of this sequence, going left to right
    with the start value z on the left:

    op(op(op(z, x_1), x_2), ..., x_n)

    where x1, ..., xn are the elements of this sequence.
    */

    // Since `append` takes time proportional to its first argument, and this first argument never grows because of the
    // right-associativity of `foldRight`, this function is linear in the total length of all lists.

    import Exercise_3_10.foldLeft

    // This is slower, as the first argument to appendList is the accumulator
    def concatenate[A](ll: List[List[A]]): List[A] =
      foldLeft(ll, List[A]())(appendList)

    // This is faster, as the first argument to appendList is the list
    def concatenate2[A](ll: List[List[A]]): List[A] =
      foldRight(ll, List[A]())(appendList)
  }

  object Exercise_3_16 {
    def add1ToEachElement(l: List[Int]): List[Int] = l match {
      case Cons(h, t) => Cons(h + 1, add1ToEachElement(t))
      case _ => Nil
    }

    def add2ToEachElement(l: List[Int]): List[Int] =
      foldRight(l, List[Int]())((h, acc) => Cons(h + 2, acc))
  }

  object Exercise_3_17 {
    def doubleToString(l: List[Double]): List[String] =
      foldRight(l, List[String]())((h, acc) => Cons(h.toString, acc))
  }

  object Exercise_3_18 {
    import Exercise_3_12.reverse
    import Exercise_3_13.foldRightViaFoldLeft

    def map[A, B](l: List[A])(f: A => B): List[B] =
      foldRight(l, List[B]())((h, acc) => Cons(f(h), acc))

    def map1[A, B](l: List[A])(f: A => B): List[B] =
      foldRightViaFoldLeft(l, List[B]())((h, acc) => Cons(f(h), acc))

    def map2[A, B](l: List[A])(f: A => B): List[B] = {
      var newList: List[B] = Nil
      @tailrec
      def go(l: List[A]): List[B] = l match {
        case Cons(h, t) => newList = Cons(f(h), newList); go(t)
        case _ => reverse(newList)
      }
      go(l)
    }

    def map3[A, B](l: List[A])(f: A => B): List[B] = {
      val buf = new collection.mutable.ListBuffer[B]

      @scala.annotation.tailrec
      def go(l: List[A]): Unit = l match {
        case Nil => ()
        case Cons(h, t) => buf += f(h); go(t)
      }
      go(l)

      List(buf.toList: _*) // converting from the standard Scala list to the list we've defined here
    }
  }

  object Exercise_3_19 {
    import Exercise_3_12.reverse
    import Exercise_3_13.foldRightViaFoldLeft

    def filter[A](as: List[A])(f: A => Boolean): List[A] =
      foldRight(as, List[A]())((h, acc) => if (f(h)) Cons(h, acc) else acc)

    def filter1[A](l: List[A])(f: A => Boolean): List[A] =
      foldRightViaFoldLeft(l, List[A]())((h, acc) => if (f(h)) Cons(h, acc) else acc)

    def filter2[A](l: List[A])(f: A => Boolean): List[A] = {
      val buf = new collection.mutable.ListBuffer[A]

      @scala.annotation.tailrec
      def go(l: List[A]): Unit = l match {
        case Nil => ()
        case Cons(h, acc) => if (f(h)) buf += h; go(acc)
      }
      go(l)

      List(buf.toList: _*)
    }

    def filter3[A](as: List[A])(f: A => Boolean): List[A] = {
      @tailrec
      def go(as: List[A], filtered: List[A]): List[A] = as match {
        case Cons(h, t) => if (f(h)) go(t, Cons(h, filtered)) else go(t, filtered)
        case Nil => reverse(filtered)
      }
      go(as, Nil)
    }
  }

  object Exercise_3_20 {
    def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
      foldRight(l, List[B]())((h, acc) => foldRight(f(h), acc)(Cons(_, _)))

    def flatMap2[A, B](l: List[A])(f: A => List[B]): List[B] =
      foldRight(l, List[B]())((h, acc) => Exercise_3_14.appendList(f(h), acc))

    import Exercise_3_15.concatenate2
    import Exercise_3_18.map

    def flatMap3[A, B](l: List[A])(f: A => List[B]): List[B] =
      concatenate2(map(l)(f))
  }

  object Exercise_3_21 {
    import Exercise_3_20.flatMap3

    def filter[A](as: List[A])(f: A => Boolean): List[A] =
      flatMap3(as)(a => if (f(a)) List(a) else Nil)
  }

  object Exercise_3_22 {
    // TODO - Check all these...
    import Exercise_3_12.reverse

    def zipWithInt(a: List[Int], b: List[Int]): List[Int] = {
      @scala.annotation.tailrec
      def go(x: List[Int], y: List[Int], acc: List[Int]): List[Int] = {
        x match {
          case Cons(h1, t1) => y match {
            case Cons(h2, t2) =>
              go(t1, t2, Cons(h1 + h2, acc))

            case Nil => appendList(reverse(acc), x)
          }

          case Nil => appendList(reverse(acc), y)
        }
      }
      go(a, b, Nil)
    }

    // You can also (somewhat less conveniently, but a bit more efficiently) nest pattern matches: on the right hand
    // side of the `=>`, simply begin another `match` expression. The inner `match` will have access to all the
    // variables introduced in the outer `match`.
    def zipWithInt2(a: List[Int], b: List[Int]): List[Int] = {
      a match {
        case Cons(h1, t1) => b match {
          case Cons(h2, t2) =>
            Cons(h1 + h2, zipWithInt2(t1, t2))

          case Nil => Nil
        }
        case Nil => Nil
      }
    }

    // To match on multiple values, we can put the values into a pair and match on the pair
    // The discussion about stack usage from the explanation of `map` also applies here.
    def zipWithInt3(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1,t1), Cons(h2,t2)) => Cons(h1+h2, zipWithInt3(t1,t2))
    }

    import Exercise_3_18._
    def zipWithIntMapViaFoldRight(l: List[Int], r: List[Int]): List[Int] = {
      println()
      println(s"zipWithIntMapViaFoldRight($l, $r)")

      var head = reverse(r)
      map(l)(a => {
        val inc = head match {
          case Cons(h, t) => head = t; h
          case _ => 0
        }
        println(s"l [$a] r [$inc]")
        a + inc
      })
    }

    def zipWithMap2(l: List[Int], r: List[Int]): List[Int] = {
      println()
      println(s"zipWithMap2($l, $r)")

      var head = r
      map2(l)(a => {
        val inc = head match {
          case Cons(h, t) => head = t; h
          case _ => 0
        }
        println(s"a [$a] inc [$inc]")
        a + inc
      })
    }

    import Exercise_3_10.foldLeft
    def mapViaFoldLeft[A, B](l: List[A])(f: A => B): List[B] =
      reverse(foldLeft(l, List[B]())((acc, h) => Cons(f(h), acc)))

    def zipWithMapViaFoldLeft(l: List[Int], r: List[Int]): List[Int] = {
      println()
      println(s"zipWithMapViaFoldLeft($l, $r)")

      var head = r
      mapViaFoldLeft(l)(a => {
        val inc = head match {
          case Cons(h, t) => head = t; h
          case _ => 0
        }
        println(s"a [$a] inc [$inc]")
        a + inc
      })
    }
  }

  object Exercise_3_23 {
    // The discussion about stack usage from the explanation of `map` also applies here.
    // By putting the `f` in the second argument list, Scala can infer its type from the previous argument list.
    def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = (a, b) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    }
  }

  object Exercise_3_24 {
    @scala.annotation.tailrec
    def startsWith[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
      case (_, Nil) => true
      case (Nil, _) => false
      case (Cons(h1, t1), Cons(h2, t2)) => if (h1 == h2) startsWith(t1, t2) else false
    }

    @tailrec
    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
        case Nil => sub == Nil
        case Cons(_, t) => startsWith(sup, sub) || hasSubsequence(t, sub)
    }

    // It's good to specify some properties about these functions up front.
    // For example, do you expect these expressions to be true?

    // (xs append ys) startsWith xs
    // xs startsWith Nil
    // (xs append ys append zs) hasSubsequence ys
    // xs hasSubsequence Nil

    // You will find that if the answer to any one of these is "yes", then
    // that implies something about the answer to the rest of them.

    @annotation.tailrec
    def startsWithTextbook[A](l: List[A], prefix: List[A]): Boolean = (l,prefix) match {
      case (_,Nil) => true
      case (Cons(h,t),Cons(h2,t2)) if h == h2 => startsWithTextbook(t, t2)
      case _ => false
    }

    @annotation.tailrec
    def hasSubsequenceTextbook[A](sup: List[A], sub: List[A]): Boolean = sup match {
      case Nil => sub == Nil
      case _ if startsWith(sup, sub) => true
      case Cons(_,t) => hasSubsequenceTextbook(t, sub)
    }
  }

  object ListWorkout {
    def main(args: Array[String]): Unit = {
      println(s"sum(List(1, 5, 9)) = ${sum(List(1, 5, 9))}")
      println(s"product(List(1, 5, 9)) = ${product(List(1, 5, 9))}")
      println(s"append(List(1, 2, 3), List(4, 5)) = ${appendList(List(1, 2, 3), List(4, 5))}")
      println(s"sum2(List(1, 5, 9)) = ${sum2(List(1, 5, 9))}")
      println(s"product2(List(1, 5, 9)) = ${product2(List(1, 5, 9))}")
      println(s"Seq(1, 2, 3).foldRight(10)(_ - _) [(1 - (2 - (3 - 10)))] = ${Seq(1, 2, 3).foldRight(10)(_ - _)}")
      println(s"foldRight(List(1, 2, 3), 10)(_ - _) [(1 - (2 - (3 - 10)))] = ${foldRight(List(1, 2, 3), 10)(_ - _)}")
    }
  }
}

object ListTestExercises {

  import List._

  def testExercise3_1(): Unit = {
    println("Exercise 3.1, pattern match")
    println("===========================")

    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }
    println(x)
    println()
  }

  def testExercise3_2(): Unit = {
    import Exercise_3_2._

    println("Exercise 3.2, tail")
    println("==================")

    println(s"tail(List(1,2,3)) = ${tail(List(1, 2, 3))}")
    println(s"tail(List(3)) = ${tail(List(3))}")
    println()
  }

  def testExercise3_3(): Unit = {
    import Exercise_3_3._

    println("Exercise 3.3, setHead")
    println("=====================")

    println(s"setHead(List(1, 2, 3), 4) = ${setHead(List(1, 2, 3), 4)}")
    println()
  }

  def testExercise3_4(): Unit = {
    import Exercise_3_4._

    println("Exercise 3.4, drop")
    println("==================")

    println(s"drop(List(1, 2, 3), -1) = ${drop(List(1, 2, 3), -1)}")
    println(s"drop(List(1, 2, 3), 0) = ${drop(List(1, 2, 3), 0)}")
    println(s"drop(List(1, 2, 3), 1) = ${drop(List(1, 2, 3), 1)}")
    println(s"drop(List(1, 2, 3), 2) = ${drop(List(1, 2, 3), 2)}")
    println(s"drop(List(1, 2, 3), 3) = ${drop(List(1, 2, 3), 3)}")
    println(s"drop(List(1, 2, 3), 4) = ${drop(List(1, 2, 3), 4)}")
    println(s"drop(Nil, -1) = ${drop(Nil, -1)}")
    println(s"drop(Nil, 0) = ${drop(Nil, 0)}")
    println(s"drop(Nil, 1) = ${drop(Nil, 1)}")
    println()
  }

  def testExercise3_5(): Unit = {
    import Exercise_3_5._

    println("Exercise 3.5, dropWhile")
    println("=======================")

    println(s"dropWhile(List(1, 2, 3, 4), (x: Int) => x < 3) = ${dropWhile(List(1, 2, 3, 4), (x: Int) => x < 3)}")
    println(s"dropWhile[Int](List(1, 2, 3, 4), _ > 3) = ${dropWhile[Int](List(1, 2, 3, 4), _ > 3)}")
    println(s"dropWhile[Int](Nil, _ > 3) = ${dropWhile[Int](Nil, _ > 3)}")
    println()

    println(s"dropWhileTextbook(List(1, 2, 3, 4), (x: Int) => x < 3) = ${dropWhileTextbook(List(1, 2, 3, 4), (x: Int) => x < 3)}")
    println(s"dropWhileTextbook[Int](List(1, 2, 3, 4), _ > 3) = ${dropWhileTextbook[Int](List(1, 2, 3, 4), _ > 3)}")
    println(s"dropWhileTextbook[Int](Nil, _ > 3) = ${dropWhileTextbook[Int](Nil, _ > 3)}")
    println()

    println(s"dropWhile2(List(1, 2, 3, 4))((x) => x < 3) = ${dropWhile2(List(1, 2, 3, 4))((x) => x < 3)}")
    println(s"dropWhile2(List(1, 2, 3, 4))(_ > 3) = ${dropWhile2(List(1, 2, 3, 4))(_ > 3)}")
    println(s"dropWhile2[Int](Nil)(_ > 3) = ${dropWhile2[Int](Nil)(_ > 3)}")
    println()
  }

  def testExercise3_6(): Unit = {
    import Exercise_3_6._

    println("Exercise 3.6, init")
    println("==================")

    def testInit(initFnName: String, init: List[Any] => List[Any]): Unit = {
      println(s"Testing $initFnName:")

      println(s"$initFnName(List(1, 2, 3)) = ${init(List(1, 2, 3))}")
      println(s"$initFnName(List(1, 2)) = ${init(List(1, 2))}")
      println(s"$initFnName(List(1)) = ${init(List(1))}")
    }

    testInit("init", init)
    testInit("init2", init2)
    testInit("initReverse", initReverse)

    println()
  }

  def testExercise3_8(): Unit = {
    println("Exercise 3.8, foldRight")
    println("=======================")

    println(s"foldRight(List(1, 2, 3), Nil:List[Int])(Cons(_, _)) = ${foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _))}")

    println()
  }

  def testExercise3_9(): Unit = {
    import Exercise_3_9._

    println("Exercise 3.9, length")
    println("====================")

    def lengthFoldRight[A](l: List[A]): Int = {
      foldRight(l, 0)((_, acc) => acc + 1)
    }

    def testLength(lengthFnName: String, length: List[Any] => Int): Unit = {
      println(s"Testing $lengthFnName:")

      println(s"$lengthFnName(List(1, 2, 3)) = ${length(List(1, 2, 3))}")
      println(s"$lengthFnName(List(1, 2)) = ${length(List(1, 2))}")
      println(s"$lengthFnName(List(1)) = ${length(List(1))}")
      println(s"$lengthFnName(List()) = ${length(List())}")
      println()
    }

    def testLength2(lengthFnName: String, length: List[Any] => Int): Unit = {
      println(s"Testing $lengthFnName:")

      println(s"$lengthFnName(List(3, 2, 1)) = ${length(List(3, 2, 1))}")
      println(s"$lengthFnName(List(2, 1)) = ${length(List(2, 1))}")
      println(s"$lengthFnName(List(1)) = ${length(List(1))}")
      println(s"$lengthFnName(List()) = ${length(List())}")
      println()
    }

    testLength("lengthRecursive", lengthRecursive)
    testLength("length", length)
    testLength("lengthIncorrect1", lengthIncorrect1)

    testLength2("lengthIncorrect1", lengthIncorrect1)
    testLength2("lengthIncorrect2", lengthIncorrect2)
    testLength2("lengthFoldRight", lengthFoldRight)

    println()
  }

  def testExercise3_10(): Unit = {
    import Exercise_3_10._

    println("Exercise 3.10, foldLeft")
    println("=======================")

    def lengthFoldLeftNotTailRecursive[A](l: List[A]): Int = {
      foldLeftNotTailRecursive(l, 0)((acc, _) => acc + 1)
    }

    println(s"lengthFoldLeftNotTailRecursive(List(1, 2, 3)) = ${lengthFoldLeftNotTailRecursive(List(1, 2, 3))}")
    println(s"lengthFoldLeftNotTailRecursive(List(1, 2)) = ${lengthFoldLeftNotTailRecursive(List(1, 2))}")
    println(s"lengthFoldLeftNotTailRecursive(List(1)) = ${lengthFoldLeftNotTailRecursive(List(1))}")
    println(s"lengthFoldLeftNotTailRecursive(List()) = ${lengthFoldLeftNotTailRecursive(List())}")
    println()

    println(s"foldLeftNotTailRecursive(List(1, 2, 3), 10)(_ - _) [10 - 1 - 2 - 3] = ${foldLeftNotTailRecursive(List(1, 2, 3), 10)(_ - _)}")
    println(s"foldLeft(List(1, 2, 3), 10)(_ - _) [10 - 1 - 2 - 3] = ${foldLeft(List(1, 2, 3), 10)(_ - _)}")
    println(s"foldRightTailRec(List(1, 2, 3), 10)(_ - _) [(1 - (2 - (3 - 10)))] => -8, Actually [(3 - (2 - (1 - 10)))] => -8 = ${foldRightTailRec(List(1, 2, 3), 10)(_ - _)}")
    println(s"foldRightTailRec(List(1, 2, 3, 4), 10)(_ - _) [(1 - (2 - (3 - (4 - 10))))] => 8, Actually [(4 - (3 - (2 - (1 - 10))))] => 12 = ${foldRightTailRec(List(1, 2, 3, 4), 10)(_ - _)}")
    println()

    val aLongList: List[Int] = constructList(10000)
    // java.lang.StackOverflowError!
    // println(s"Length of 10000 element list, foldLeftNotTailRecursive = ${foldLeftNotTailRecursive(aLongList, 0)((acc, _) => acc + 1)}")

    println(s"Length of 10000 element list, foldLeft = ${foldLeft(aLongList, 0)((acc, _) => acc + 1)}")

    // java.lang.StackOverflowError!
    // println(s"Length of 10000 element list, foldRight = ${List.foldRight(aLongList, 0)((_, acc) => acc + 1)}")

    println(s"Length of 10000 element list, tailrec foldRightTailRec = ${foldRightTailRec(aLongList, 0)((_, acc) => acc + 1)}")

    println()
  }

  def testExercise3_11(): Unit = {
    import Exercise_3_11._

    println("Exercise 3.11, sum, product, length via foldLeft")
    println("================================================")

    println(s"sum(List(1, 5, 9)) = ${sum(List(1, 5, 9))}")

    println(s"product(List(1, 5, 9)) = ${product(List(1, 5, 9))}")

    println(s"length(List(1, 2, 3)) = ${length(List(1, 2, 3))}")
    println(s"length(List(1, 2)) = ${length(List(1, 2))}")
    println(s"length(List(1)) = ${length(List(1))}")
    println(s"length(List()) = ${length(List())}")
    println()
  }

  def testExercise3_12(): Unit = {
    import Exercise_3_12._

    println("Exercise 3.12, reverse")
    println("======================")

    def testReverse(recursiveFnName: String, recursive: List[Any] => List[Any]): Unit = {
      println(s"Testing $recursiveFnName:")

      println(s"$recursiveFnName(List(1, 2, 3)) = ${recursive(List(1, 2, 3))}")
      println(s"$recursiveFnName(List(C, B, A)) = ${recursive(List('C', 'B', 'A'))}")
      println()
    }

    testReverse("reverseRecursive", reverseRecursive)
    testReverse("reverse", reverse)
  }

  def testExercise3_13(): Unit = {
    import Exercise_3_13._

    println("Exercise 3.13, folds from folds")
    println("===============================")

    println(s"foldLeftViaRightFold(List(6, 9, 4), 0)(_ - _) [0 - 6 - 9 - 4] = ${foldLeftViaFoldRight(List(6, 9, 4), 0)(_ - _)}")
    println(s"foldLeftViaRightFold(List(1, 2, 3), 10)(_ - _) [10 - 1 - 2 - 3] = ${foldLeftViaFoldRight(List(1, 2, 3), 10)(_ - _)}")
    println(s"foldLeftViaRightFold(List(1, 2, 3, 4), 10)(_ - _) [10 - 1 - 2 - 3 - 4] = ${foldLeftViaFoldRight(List(1, 2, 3, 4), 10)(_ - _)}")

    println(s"foldRightViaLeftFold(List(6, 9, 4), 0)(_ - _) [(6 - (9 - (4 - 0)))] = ${foldRightViaFoldLeft(List(6, 9, 4), 0)(_ - _)}")
    println(s"foldRightViaLeftFold(List(1, 2, 3), 10)(_ - _) [(1 - (2 - (3 - 10)))] = ${foldRightViaFoldLeft(List(1, 2, 3), 10)(_ - _)}")
    println(s"foldRightViaLeftFold(List(1, 2, 3, 4), 10)(_ - _) [(1 - (2 - (3 - (4 - 10))))] = ${foldRightViaFoldLeft(List(1, 2, 3, 4), 10)(_ - _)}")

    val aLongList: List[Int] = constructList(10000)
    // java.lang.StackOverflowError!
    // println(s"Length of 10000 element list, foldLeftViaRightFold = ${foldLeftViaRightFold(aLongList, 0)((acc, _) => acc + 1)}")

    println(s"Length of 10000 element list, foldRightViaLeftFold = ${foldRightViaFoldLeft(aLongList, 0)((_, acc) => acc + 1)}")

    println()
  }

  def testExercise3_14(): Unit = {
    import Exercise_3_14._

    println("Exercise 3.14, append")
    println("=====================")

    def testAppend(appendFnName: String, append: (List[Any], Any) => List[Any]): Unit = {
      println(s"Testing $appendFnName:")

      val a1 = append(List[Int](), 0)
      val a2 = append(a1, 1)
      val a3 = append(a2, 2)
      val a4 = append(a3, 3)
      println(s"$appendFnName(List(), 0) = $a1}")
      println(s"$appendFnName(List(0), 1) = $a2}")
      println(s"$appendFnName(List(0, 1), 2) = $a3")
      println(s"$appendFnName(List(0, 1, 2), 3) = $a4")
      println()
    }

    testAppend("appendElem", appendElem)
    testAppend("appendElem2", appendElem)

    println(s"append(List(1, 2, 3), List(4, 5)) = ${appendList(List(1, 2, 3), List(4, 5))}")
    println()
  }

  def testExercise3_15(): Unit = {
    import Exercise_3_15._

    println("Exercise 3.15, concatenate")
    println("==========================")

    def testConcatenate(concatenateFnName: String, concatenate: List[List[Any]] => List[Any]): Unit = {
      println(s"Testing $concatenateFnName:")

      println(s"$concatenateFnName(L(L(1, 2, 3), L(4, 5, 6), L(7, 8, 9))) = ${concatenate(List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)))}")
      println(s"$concatenateFnName(L(L(1, 2, 3), L(4, 5, 6), L(7, 8, 9), L(10, 11, 12))) = ${concatenate(List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9), List(10, 11, 12)))}")
      println()
    }

    testConcatenate("concatenate", concatenate)
    testConcatenate("concatenate2", concatenate2)
  }

  def testExercise3_16(): Unit = {
    import Exercise_3_16._

    println("Exercise 3.16, add to each element")
    println("==================================")

    def testAddToEachElement(addToEachElementFnName: String, addToEachElement: List[Int] => List[Int]): Unit = {
      println(s"Testing $addToEachElementFnName:")

      println(s"$addToEachElementFnName(List(1, 2, 3)) = ${addToEachElement(List(1, 2, 3))}")
      println(s"$addToEachElementFnName(List(1, 2, 3, 4)) = ${addToEachElement(List(1, 2, 3, 4))}")
      println()
    }

    testAddToEachElement("add1ToEachElement", add1ToEachElement)
    testAddToEachElement("add2ToEachElement", add2ToEachElement)
  }

  def testExercise3_17(): Unit = {
    import Exercise_3_17._

    println("Exercise 3.17, double to string")
    println("===============================")

    println(s"doubleToString(List(1, 2, 3)) = ${doubleToString(List(1d, 2d, 3d))}")

    println()
  }

  def testExercise3_18(): Unit = {
    import Exercise_3_18._

    println("Exercise 3.18, map")
    println("==================")

    println(s"add2ToEachElement(List(1, 2, 3)) = ${map(List(1, 2, 3))(_ + 2)}")
    println(s"doubleToString(List(1, 2, 3)) = ${map(List(1d, 2d, 3d))(_.toString)}")
    println(s"add2ToEachElement(List(1, 2, 3)) = ${map1(List(1, 2, 3))(_ + 2)}")
    println(s"doubleToString(List(1, 2, 3)) = ${map1(List(1d, 2d, 3d))(_.toString)}")
    println(s"add2ToEachElement(List(1, 2, 3)) = ${map2(List(1, 2, 3))(_ + 2)}")
    println(s"doubleToString(List(1, 2, 3)) = ${map2(List(1d, 2d, 3d))(_.toString)}")
    println(s"add2ToEachElement(List(1, 2, 3)) = ${map3(List(1, 2, 3))(_ + 2)}")
    println(s"doubleToString(List(1, 2, 3)) = ${map3(List(1d, 2d, 3d))(_.toString)}")
    println()
  }

  def testExercise3_19(): Unit = {
    import Exercise_3_19._

    println("Exercise 3.19, filter")
    println("=====================")

    println(s"removeOddNumbers(List(1, 2, 3, 4, 5)) = ${filter(List(1, 2, 3, 4, 5))(_ % 2 == 0)}")
    println(s"removeOddNumbers(List(1, 2, 3, 4, 5)) = ${filter1(List(1, 2, 3, 4, 5))(_ % 2 == 0)}")
    println(s"removeOddNumbers(List(1, 2, 3, 4, 5)) = ${filter2(List(1, 2, 3, 4, 5))(_ % 2 == 0)}")
    println(s"removeOddNumbers(List(1, 2, 3, 4, 5)) = ${filter3(List(1, 2, 3, 4, 5))(_ % 2 == 0)}")
    println()
  }

  def testExercise3_20(): Unit = {
    import Exercise_3_20._

    println("Exercise 3.20, flatMap")
    println("======================")

    println(s"flatMap(List(1, 2, 3))(i => List(i, i)) = ${flatMap(List(1, 2, 3))(i => List(i, i))}")
    println(s"flatMap(List(1, 2, 3))(i => List(i, i)) = ${flatMap2(List(1, 2, 3))(i => List(i, i))}")
    println(s"flatMap(List(1, 2, 3))(i => List(i, i)) = ${flatMap3(List(1, 2, 3))(i => List(i, i))}")
    println()
  }

  def testExercise3_21(): Unit = {
    import Exercise_3_21._

    println("Exercise 3.19, filter via flatMap")
    println("=================================")

    println(s"removeOddNumbers(List(1, 2, 3, 4, 5)) = ${filter(List(1, 2, 3, 4, 5))(_ % 2 == 0)}")
    println()
  }

  def testExercise3_22(): Unit = {
    import Exercise_3_22._

    println("Exercise 3.22, zipWithInt")
    println("=========================")

    def testZipWithInt(zipWithIntFnName: String, zipWithInt: (List[Int], List[Int]) => List[Int]): Unit = {
      println(s"Testing $zipWithIntFnName:")

      println(s"$zipWithIntFnName(List(1, 2, 3), List(4, 5, 6)) = ${zipWithInt(List(1, 2, 3), List(4, 5, 6))}")
      println(s"$zipWithIntFnName(Nil, List(4, 5, 6)) = ${zipWithInt(Nil, List(4, 5, 6))}")
      println(s"$zipWithIntFnName(List(1, 2, 3), Nil) = ${zipWithInt(List(1, 2, 3), Nil)}")
      println(s"$zipWithIntFnName(List(1, 2, 3, 4, 5), List(4, 5, 6)) = ${zipWithInt(List(1, 2, 3, 4, 5), List(4, 5, 6))}")
      println(s"$zipWithIntFnName(List(1, 2, 3), List(4, 5, 6, 7, 8)) = ${zipWithInt(List(1, 2, 3), List(4, 5, 6, 7, 8))}")
      println()
    }

    testZipWithInt("zipWithInt", zipWithInt)
    testZipWithInt("zipWithInt2", zipWithInt2)
    testZipWithInt("zipWithInt3", zipWithInt3)
    testZipWithInt("zipWithIntMapViaFoldRight", zipWithIntMapViaFoldRight)
    testZipWithInt("zipWithMap2", zipWithMap2)
    testZipWithInt("zipWithMapViaFoldLeft", zipWithMapViaFoldLeft)
  }

  def testExercise3_23(): Unit = {
    import Exercise_3_23._

    println("Exercise 3.23, zipWith")
    println("======================")

    println("zipWith(Cons(1, Cons(2, Cons(3, Nil))), Cons(4, Cons(5, Cons(6, Nil))))(_ + _) = " +
      zipWith(Cons(1, Cons(2, Cons(3, Nil))), Cons(4, Cons(5, Cons(6, Nil))))(_ + _)
    )

    println("zipWith(Cons(1, Cons(2, Cons(3, Nil))), Cons(4, Cons(5, Cons(6, Nil))))(_ - _) = " +
      zipWith(Cons(1, Cons(2, Cons(3, Nil))), Cons(4, Cons(5, Cons(6, Nil))))(_ - _)
    )

    println("""zipWith(Cons("a", Cons("b", Cons("c", Nil))), Cons("x", Cons("y", Cons("z", Nil))))(_ + _) = """ +
      zipWith(Cons("a", Cons("b", Cons("c", Nil))), Cons("x", Cons("y", Cons("z", Nil))))(_ + _)
    )

    println()
  }

  def testExercise3_24(): Unit = {
    import Exercise_3_24._

    println("Exercise 3.24, hasSubsequence")
    println("=============================")

    def testHasSubsequence(hasSubsequenceFnName: String, hasSubsequence: (List[Any], List[Any]) => Boolean): Unit = {
      println(s"Testing $hasSubsequenceFnName:")

      println(s"$hasSubsequenceFnName(Cons(1, Cons(2, Cons(3, Cons(4, Nil)))), Cons(1, Cons(2, Cons(3, Cons(4, Nil))))) = " +
        hasSubsequence(Cons(1, Cons(2, Cons(3, Cons(4, Nil)))), Cons(1, Cons(2, Cons(3, Cons(4, Nil)))))
      )

      println(s"$hasSubsequenceFnName(Cons(1, Cons(2, Cons(3, Cons(4, Nil)))), Cons(1, Cons(2, Cons(3, Nil)))) = " +
        hasSubsequence(Cons(1, Cons(2, Cons(3, Cons(4, Nil)))), Cons(1, Cons(2, Cons(3, Nil))))
      )

      println(s"$hasSubsequenceFnName(Cons(1, Cons(2, Cons(3, Cons(4, Nil)))), Cons(2, Cons(3, Cons(4, Nil)))) = " +
        hasSubsequence(Cons(1, Cons(2, Cons(3, Cons(4, Nil)))), Cons(2, Cons(3, Cons(4, Nil))))
      )

      println(s"$hasSubsequenceFnName(Cons(1, Cons(2, Cons(3, Cons(4, Nil)))), Cons(2, Cons(3, Nil))) = " +
        hasSubsequence(Cons(1, Cons(2, Cons(3, Cons(4, Nil)))), Cons(2, Cons(3, Nil)))
      )

      println(s"$hasSubsequenceFnName(Cons(1, Cons(2, Cons(3, Cons(4, Nil)))), Cons(4, Nil)) = " +
        hasSubsequence(Cons(1, Cons(2, Cons(3, Cons(4, Nil)))), Cons(4, Nil))
      )

      println(s"$hasSubsequenceFnName(Cons(1, Cons(2, Cons(3, Cons(4, Nil)))), Cons(3, Nil)) = " +
        hasSubsequence(Cons(1, Cons(2, Cons(3, Cons(4, Nil)))), Cons(3, Nil))
      )

      println(s"$hasSubsequenceFnName(Cons(1, Cons(2, Cons(3, Cons(4, Nil)))), Cons(2, Cons(4, Nil))) = " +
        hasSubsequence(Cons(1, Cons(2, Cons(3, Cons(4, Nil)))), Cons(2, Cons(4, Nil)))
      )

      println(s"$hasSubsequenceFnName(Cons(2, Cons(3, Cons(2, Cons(3, Cons(4, Nil))))), Cons(2, Cons(3, Cons(4, Nil)))) = " +
        hasSubsequence(Cons(2, Cons(3, Cons(2, Cons(3, Cons(4, Nil))))), Cons(2, Cons(3, Cons(4, Nil))))
      )

      println(s"$hasSubsequenceFnName(Cons(2, Cons(3, Cons(2, Cons(3, Cons(4, Nil))))), Nil) = " +
        hasSubsequence(Cons(2, Cons(3, Cons(2, Cons(3, Cons(4, Nil))))), Nil)
      )

      println(s"$hasSubsequenceFnName(Nil, Nil) = " +
        hasSubsequence(Nil, Nil)
      )

      println()
    }

    def testStartsWith(startsWithFnName: String, startsWith: (List[Any], List[Any]) => Boolean): Unit = {
      println(s"Testing $startsWithFnName:")

      val xs = Cons(1, Cons(2, Cons(3, Nil)))
      val ys = Cons(4, Nil)
      val xs_con_ys = appendList(xs, ys)

      println(s"$startsWithFnName($xs, Nil) = ${startsWith(xs, Nil)}")
      println(s"$startsWithFnName($xs_con_ys, $xs) = ${startsWith(xs_con_ys, xs)}")
      println(s"$startsWithFnName($xs_con_ys, $ys) = ${startsWith(xs_con_ys, ys)}")
      println(s"$startsWithFnName($xs, $xs_con_ys) = ${startsWith(xs, xs_con_ys)}")
      println(s"$startsWithFnName($xs, $xs) = ${startsWith(xs, xs)}")
      println(s"$startsWithFnName(Nil, Nil) = ${startsWith(Nil, Nil)}")
      println(s"$startsWithFnName(Nil, $xs) = ${startsWith(Nil, xs)}")
      println(s"$startsWithFnName($xs, Cons(1, Cons(2, Nil))) = ${startsWith(xs, Cons(1, Cons(2, Nil)))}")
      println(s"$startsWithFnName($xs, Cons(1, Nil)) = ${startsWith(xs, Cons(1, Nil))}")
      println(s"$startsWithFnName($xs, Cons(2, Cons(3, Nil))) = ${startsWith(xs, Cons(2, Cons(3, Nil)))}")
      println()
    }

    testStartsWith("startsWith", startsWith)
    testHasSubsequence("hasSubsequence", hasSubsequence)
    testStartsWith("startsWithTextbook", startsWithTextbook)
    testHasSubsequence("hasSubsequenceTextbook", hasSubsequenceTextbook)

    println()
  }


  def main(args: Array[String]): Unit = {
//    testExercise3_1()
//    testExercise3_2()
//    testExercise3_3()
//    testExercise3_4()
//    testExercise3_5()
//    testExercise3_6()
//    testExercise3_8()
//    testExercise3_9()
//    testExercise3_10()
//    testExercise3_11()
//    testExercise3_12()
//    testExercise3_13()
//    testExercise3_14()
//    testExercise3_15()
//    testExercise3_16()
//    testExercise3_17()
//    testExercise3_18()
//    testExercise3_19()
//    testExercise3_20()
//    testExercise3_21()
    testExercise3_22()
//    testExercise3_23()
//    testExercise3_24()
  }
}