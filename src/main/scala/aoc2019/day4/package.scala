package aoc2019

import aoc2019.parser.Parser

package object day4 {
  implicit val parser: Parser[Range] = Parser.range
}
