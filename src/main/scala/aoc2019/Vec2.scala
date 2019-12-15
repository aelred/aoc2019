package aoc2019

case class Vec2(x: Int, y: Int) {
  def +(vec: Vec2): Vec2 = Vec2(x + vec.x, y + vec.y)
  def -(vec: Vec2): Vec2 = Vec2(x - vec.x, y - vec.y)
  def *(value: Int): Vec2 = Vec2(x * value, y * value)
  def /(value: Int): Vec2 = Vec2(x / value, y / value)

  def angle: Double = -math.atan2(x, y)
}
