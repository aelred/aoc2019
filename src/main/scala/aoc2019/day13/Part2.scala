package aoc2019.day13

import aoc2019._
import aoc2019.program.Program
import scala.concurrent.duration._

import scala.collection.mutable

object Part2 extends Solution[Program] {
  override protected def solution: Long = {
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
}
