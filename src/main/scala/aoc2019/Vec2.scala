package aoc2019

case class Vec2(x: Int, y: Int) {
  def +(vec: Vec2): Vec2 = Vec2(x + vec.x, y + vec.y)
  def -(vec: Vec2): Vec2 = Vec2(x - vec.x, y - vec.y)
  def *(value: Int): Vec2 = Vec2(x * value, y * value)
  def /(value: Int): Vec2 = Vec2(x / value, y / value)

  def angle: Double = -math.atan2(x, y)

  def shift(direction: Direction): Vec2 = direction.shift(this)

  def left: Vec2 = shift(Direction.L())
  def right: Vec2 = shift(Direction.R())
  def up: Vec2 = shift(Direction.U())
  def down: Vec2 = shift(Direction.D())
  def neighbours: Set[Vec2] = Direction.all.map(shift)
}

object Vec2 {
  val zero = Vec2(0, 0)
}