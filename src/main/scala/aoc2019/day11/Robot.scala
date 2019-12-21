package aoc2019.day11

import aoc2019.program.Program
import aoc2019.{Direction, Vec2, Up}

import scala.collection.mutable

object Robot {

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
