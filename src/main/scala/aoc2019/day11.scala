package aoc2019

import aoc2019.program.Program

import scala.collection.mutable

object day11 extends Solution[Program] {

  def part1: Int = {
    val hull = run(input)
    hull.keys.size
  }

  def part2: String = {
    val hull = run(input)

    mapToString(hull.view.mapValues {
      case White => '█'
      case Black => ' '
    })
  }

  val Black = 0L
  val White = 1L

  val TurnLeft = 0L
  val TurnRight = 1L

  def run(program: Program): Map[Vec2, Long] = {
    var robotPos = Vec2(0, 0)
    var robotDir: Direction = Up()

    val hull = mutable.Map[Vec2, Long]().withDefaultValue(Black)
    hull(robotPos) = White

    program.run(() => hull(robotPos)) { next =>
      hull(robotPos) = next()
      robotDir = next() match {
        case TurnLeft => robotDir.anticlockwise
        case TurnRight => robotDir.clockwise
      }
      robotPos = robotDir.shift(robotPos)
    }

    hull.toMap
  }
}
