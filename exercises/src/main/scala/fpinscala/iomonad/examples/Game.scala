package fpinscala.iomonad.examples

import fpinscala.iomonad.IO0
import fpinscala.iomonad.IO0.PrintLine
import fpinscala.monoids.Monoid

object Game {
  case class Player(name: String, score: Int)

  def contest(p1: Player, p2: Player): Unit =
    if (p1.score > p2.score)
      println(s"${p1.name} is the winner!")
    else if (p2.score > p1.score)
      println(s"${p2.name} is the winner!")
    else
      println("It's a draw!")

  // First refactor
  def winner(p1: Player, p2: Player): Option[Player] =
    if (p1.score > p2.score)
      Some(p1)
    else if (p2.score > p1.score)
      Some(p2)
    else
      None

  def contest2(p1: Player, p2: Player): Unit = {
    winner(p1, p2) match {
      case Some(Player(name, _)) =>
        println(s"$name is the winner!")
      case None =>
        println("It's a draw!")
    }
  }

  // Second refactor
  def winnerMsg(p: Option[Player]): String = (p map {
    case Player(name, _) => s"$name is the winner!"
  }).getOrElse("It's a draw!")

  def contest3(p1: Player, p2: Player): Unit =
    println(winnerMsg(winner(p1, p2)))

  // Third refactor

  def contest4(p1: Player, p2: Player): IO0.IO =
    PrintLine(winnerMsg(winner(p1, p2)))

  def main(args: Array[String]): Unit = {
    val p1 = Player("Graeme", 15)
    val p2 = Player("Matt", 25)

    println(s"Contest:")
    contest(p1, p2)

    println(s"Contest 2:")
    contest2(p1, p2)

    println(s"Contest 3:")
    contest3(p1, p2)

    println(s"Contest 4:")
    contest4(p1, p2).run

    val effectList = List(
      PrintLine("This"),
      PrintLine("is"),
      PrintLine("a"),
      PrintLine("test")
    )

    val IOMonoid =
      new IO0.IO {
        def run: Unit = () // This isn't executed
      }

    Monoid.concatenate(effectList, IOMonoid).run
  }
}
