package aoc2019.day6

import aoc2019.parser.Parser.string
import aoc2019.parser.Parser
import aoc2019.parser.Parser.Literal
import aoc2019.parser.~

case class Orbit(planet: String, satellite: String)

object Orbit {
  implicit def parser: Parser[Orbit] =
    string ~ ")" ~ string |-> { case planet ~ _ ~ satellite => Orbit(planet, satellite) }
}
