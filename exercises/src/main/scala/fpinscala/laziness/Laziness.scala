package fpinscala.laziness

object Laziness {
  def if2[A](cond: Boolean, onTrue: () => A, onFalse: () => A): A =
    if (cond) onTrue() else onFalse()

  def if2ByName[A](cond: Boolean, onTrue: => A, onFalse: => A): A =
    if (cond) onTrue else onFalse

  def maybeTwice(b: Boolean, i: => Int) =
    if (b) i+i else 0

  def maybeTwice2(b: Boolean, i: => Int) = {
    lazy val j = i
    if (b) j+j else 0
  }

  def main(args: Array[String]): Unit = {
    if2(10 < 22,
      () => println('a'),
      () => println('b')
    )

    println(
      if2ByName(false,
        sys.error("fail"),
        3
      )
    )

    val x = maybeTwice(true, { println("hi"); 1 + 41 })
    println(x)

    val x2 = maybeTwice2(true, { println("yo"); 3 + 30 })
    println(x2)
  }
}
