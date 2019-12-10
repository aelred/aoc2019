package aoc2019.day10

case class Pos(x: Int, y: Int) {
  def +(pos: Pos): Pos = Pos(x + pos.x, y + pos.y)
  def -(pos: Pos): Pos = Pos(x - pos.x, y - pos.y)
  def *(value: Int): Pos = Pos(x * value, y * value)
  def /(value: Int): Pos = Pos(x / value, y / value)
}
