package aoc2019

case class Pos(x: Int, y: Int) {
  def +(pos: Pos): Pos = Pos(x + pos.x, y + pos.y)
  def -(pos: Pos): Pos = Pos(x - pos.x, y - pos.y)
  def *(value: Int): Pos = Pos(x * value, y * value)
  def /(value: Int): Pos = Pos(x / value, y / value)

  def angle: Double = -math.atan2(x, y)
}
