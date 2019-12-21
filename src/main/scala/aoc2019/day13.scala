package aoc2019

import aoc2019.program.Program

import scala.collection.mutable
import scala.concurrent.duration._

object day13 extends Solution[Program] {

  def part1: Int = {
    val program = input
    val screen = mutable.Map[Vec2, Tile]()

    program.run() { next =>
      val pos = Vec2(next().toInt, next().toInt)
      val tile = Tile(next())

      logRaw("\u001B[0;0H")
      log(mapToString(screen.view.mapValues(_.char)))

      screen.put(pos, tile)
    }

    screen.values.count(_ == Block())
  }

  def part2: Long = {
    val program = input

    var direction = 0
    val screen = mutable.Map[Vec2, Tile]()
    var paddleX = 0
    var ballX = 0
    var score = 0L

    program.withMemory(0 -> 2).run(() => direction) { next =>
      val x = next()
      val y = next()
      val value = next()
      if (x == -1 && y == 0) {
        score = value
      } else {
        val pos = Vec2(x.toInt, y.toInt)
        val tile = Tile(value)
        screen.put(pos, tile)

        tile match {
          case Paddle() => paddleX = pos.x
          case Ball() => ballX = pos.x
          case _ => ()
        }

        direction = ballX.compare(paddleX)

        if (tile == Ball()) {
          logRaw("\u001B[0;0H")
          log(score)
          log(mapToString(screen.view.mapValues(_.char)))
          sleep(66.milliseconds)
        }
      }
    }

    score
  }

  sealed trait Tile {
    def char: Char = this match {
      case Empty() => ' '
      case Wall() => '█'
      case Block() => '#'
      case Paddle() => '='
      case Ball() => 'O'
    }
  }

  case class Empty() extends Tile
  case class Wall() extends Tile
  case class Block() extends Tile
  case class Paddle() extends Tile
  case class Ball() extends Tile

  object Tile {
    def apply(num: Long): Tile = {
      num match {
        case 0 => Empty()
        case 1 => Wall()
        case 2 => Block()
        case 3 => Paddle()
        case 4 => Ball()
      }
    }
  }
}
