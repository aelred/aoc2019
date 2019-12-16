package aoc2019.day13

import java.io.InputStreamReader

import aoc2019.program.Program
import aoc2019._

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object Part2 extends Solution[Program] {
  override protected def solution: Long = {
    val program = line

    var direction = 0

    val execution = program.withMemory(0 -> 2).start(() => direction)

    val screen = mutable.Map[Vec2, Tile]()
    var paddleX = 0
    var ballX = 0
    var score = 0L

    @tailrec
    def execute(): Unit = {
      val output = for {
        x <- execution.continue()
        y <- execution.continue()
        tileNum <- execution.continue()
      } yield {
        if (x == -1 && y == 0) {
          score = tileNum
        } else {
          val pos = Vec2(x.toInt, y.toInt)
          val tile = Tile(tileNum)
          screen.put(pos, tile)

          tile match {
            case Paddle() => paddleX = pos.x
            case Ball() => ballX = pos.x
            case _ => ()
          }

          direction = ballX.compare(paddleX)

          if (tile == Ball()) {
            print("\u001B[0;0H")
            println(score)
            println(mapToString(screen.view.mapValues(_.char)))
            Thread.sleep(66)
          }
        }
      }
      output match {
        case Some(_) => execute()
        case None =>
      }
    }

    execute()

    score
  }
}
