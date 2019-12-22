package aoc2019

import aoc2019.parser.Parser
import aoc2019.parser.Parser._

sealed trait Direction {
  def shift(pos: Vec2): Vec2
  def clockwise: Direction
  def anticlockwise: Direction
}

object Direction {

  case class U() extends Direction {
    override def shift(pos: Vec2): Vec2 = pos.copy(y = pos.y - 1)
    override def clockwise: Direction = R()
    override def anticlockwise: Direction = L()
  }

  case class D() extends Direction {
    override def shift(pos: Vec2): Vec2 = pos.copy(y = pos.y + 1)
    override def clockwise: Direction = L()
    override def anticlockwise: Direction = R()
  }

  case class L() extends Direction {
    override def shift(pos: Vec2): Vec2 = pos.copy(x = pos.x - 1)
    override def clockwise: Direction = U()
    override def anticlockwise: Direction = D()
  }

  case class R() extends Direction {
    override def shift(pos: Vec2): Vec2 = pos.copy(x = pos.x + 1)
    override def clockwise: Direction = D()
    override def anticlockwise: Direction = U()
  }

  def all: Set[Direction] = Set(U(), D(), L(), R())

  val parser: Parser[Direction] =
    "U" >> { _ => U   ()} |
    "D" >> { _ => D ()} |
    "L" >> { _ => L ()} |
    "R" >> { _ => R()}
}
