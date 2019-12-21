package aoc2019

import aoc2019.parser.Parser
import aoc2019.parser.Parser.string
import aoc2019.parser.Parser.RegexParser

package object day6 {
  private val name = "...".r
  implicit val parser: Parser[Orbit] = (name <~ ")") ~ name >> Orbit.tupled
}
