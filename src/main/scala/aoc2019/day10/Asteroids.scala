package aoc2019.day10

import aoc2019.parser.Parser
import aoc2019.parser.Parser.lit

case class Asteroids(asteroids: Seq[Boolean])

object Asteroids {
  implicit val parser: Parser[Asteroids] =
    (lit("#").map(_ => true) | lit(".").map(_ => false)).repeat.map(Asteroids(_))
}
