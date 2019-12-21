package aoc2019.day14

import aoc2019.parser.Parser
import aoc2019.parser.~
import aoc2019.parser.Parser.{int, word}

case class Material(name: String, amount: Long) {
  def *(value: Long): Material = copy(amount=amount*value)

  override def toString: String = s"$name $amount"
}

object Material {
  val parser: Parser[Material] = int ~ " " ~ word >> { case amount ~ _ ~ name => Material(name, amount)
  }
}
