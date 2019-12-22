package aoc2019

import aoc2019.parser.Parser
import aoc2019.parser.Parser._

sealed trait Direction {
  def shift(pos: Vec2): Vec2
  def clockwise: Direction
  def anticlockwise: Direction
}

object Direction {
  def all: Set[Direction] = Set(Up(), Down(), Left(), Right())

  val parser: Parser[Direction] =
    "U" >> { _ => Up   ()} |
    "D" >> { _ => Down ()} |
    "L" >> { _ => Left ()} |
    "R" >> { _ => Right()}
}


case class Up() extends Direction {
  override def shift(pos: Vec2): Vec2 = pos.copy(y = pos.y - 1)
  override def clockwise: Direction = Right()
  override def anticlockwise: Direction = Left()
}

case class Down() extends Direction {
  override def shift(pos: Vec2): Vec2 = pos.copy(y = pos.y + 1)
  override def clockwise: Direction = Left()
  override def anticlockwise: Direction = Right()
}

case class Left() extends Direction {
  override def shift(pos: Vec2): Vec2 = pos.copy(x = pos.x - 1)
  override def clockwise: Direction = Up()
  override def anticlockwise: Direction = Down()
}

case class Right() extends Direction {
  override def shift(pos: Vec2): Vec2 = pos.copy(x = pos.x + 1)
  override def clockwise: Direction = Down()
  override def anticlockwise: Direction = Up()
}
