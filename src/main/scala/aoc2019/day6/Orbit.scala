package aoc2019.day6

import aoc2019.parser.Parser.{lit, string}
import aoc2019.parser.{+, Parser}

case class Orbit(planet: String, satellite: String)

object Orbit {
  implicit def parser: Parser[Orbit] =
    (string + lit(")") + string) map { case planet + _ + satellite => Orbit(planet, satellite) }
}
