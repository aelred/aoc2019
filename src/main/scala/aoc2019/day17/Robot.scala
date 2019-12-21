package aoc2019.day17

import aoc2019.{Direction, Vec2}

case class Robot(pos: Vec2, dir: Direction) {
  def nextPos: Vec2 = pos.shift(dir)
  def move: Robot = copy(pos = nextPos)
  def left: Robot = copy(dir = dir.anticlockwise)
  def right: Robot = copy(dir = dir.clockwise)
}
