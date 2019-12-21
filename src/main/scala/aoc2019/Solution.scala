package aoc2019

import aoc2019.parser.Parser

import scala.concurrent.duration.Duration
import scala.io.Source

abstract class Solution[T: Parser] {

  protected final var input: T = _
  protected final var logging: Boolean = true

  protected def part1: Any

  protected def part2: Any

  protected final def logRaw(obj: => Any): Unit = {
    if (logging) {
      print(obj)
    }
  }

  protected final def log(obj: => Any): Unit = {
    if (logging) {
      println(obj)
    }
  }

  protected final def sleep(duration: Duration): Unit = {
    if (logging) {
      Thread.sleep(duration.toMillis)
    }
  }

  final def main(args: Array[String]): Unit = run(logging = true)

  final def run(logging: Boolean): Unit = {
    this.logging = logging

    val clazz = Option(getClass.getEnclosingClass).getOrElse(getClass)
    val path = clazz.getName.replaceAllLiterally(".", "/").stripSuffix("$")
    val name = s"$path.txt"
    val inputString = Source.fromResource(name).getLines.mkString("\n")
    input = Parser.parse(inputString).toTry.get

    println(
      s"""
         |
         | Part 1
         | ------""".stripMargin)
    println(part1)

    println(
      s"""
         |
         | Part 2
         | ------""".stripMargin)
    println(part2)
  }
}
