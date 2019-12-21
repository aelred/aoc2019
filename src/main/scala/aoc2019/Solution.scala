package aoc2019

import aoc2019.parser.Parser

import scala.concurrent.duration.Duration
import scala.io.Source

abstract class Solution[T: Parser] {

  protected final var input: T = _
  protected final var logging: Boolean = true

  protected def solution: Any

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

    val path = getClass.getPackage.getName.replaceAllLiterally(".", "/")
    val name = s"$path.txt"
    val inputString = Source.fromResource(name).getLines.mkString("\n")
    input = Parser.parse(inputString).toTry.get
    println(solution)
  }
}
