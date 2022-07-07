package fpinscala.iomonad.examples

import fpinscala.iomonad.IO1.{PrintLine, ReadLine}
import fpinscala.iomonad.{IO0, IO1}

import scala.io.StdIn.readLine

object Temperature {
  def fahrenheitToCelsius(f: Double): Double =
    (f - 32) * 5.0 / 9.0

  def converter: Unit = {
    println("Enter a temperature in degrees Fahrenheit: ")
    val d = readLine.toDouble
    println(fahrenheitToCelsius(d))
  }

  // First refactor
  def converter2: IO0.IO = IO0.PrintLine(
    "Enter a temperature in degrees Fahrenheit: "
    // now what ???
  )

  // Second refactor
  def converter3: IO1.IO[Unit] = for {
    _ <- PrintLine("Enter a temperature in degrees Fahrenheit: ")
    d <- ReadLine.map(_.toDouble)
    _ <- PrintLine(fahrenheitToCelsius(d).toString)
  } yield ()

  def main(args: Array[String]): Unit = {
    converter
    converter3.run
  }
}
