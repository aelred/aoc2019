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

    val execution = program.start(() => hull(robotPos))

    def execute(): Unit = {
      for {
        paint <- execution.continue()
        _ = hull(robotPos) = paint
        turn <- execution.continue()
      } yield {
        robotDir = turn match {
          case TurnLeft => robotDir.anticlockwise
          case TurnRight => robotDir.clockwise
        }
        robotPos = robotDir.shift(robotPos)
        execute()
      }
    }

    execute()

    hull.toMap
  }
}
