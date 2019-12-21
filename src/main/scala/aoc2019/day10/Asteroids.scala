package aoc2019.day10

import aoc2019.parser.Parser
import aoc2019.parser.Parser.Literal

case class Asteroids(asteroids: Seq[Boolean])

object Asteroids {
  implicit val parser: Parser[Asteroids] =
    ("#" >> (_ => true) | "." >> (_ => false)).repeat >> (Asteroids(_))
}
