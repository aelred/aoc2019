package aoc2019.day13

import aoc2019.day11.Robot
import aoc2019.{Solution, Vec2}
import aoc2019.mapToString
import aoc2019.program.Program

import scala.annotation.tailrec
import scala.collection.mutable

object Part1 extends Solution[Program] {
  override protected def solution: Int = {
    val program = line
    val execution = program.start(() => 0)

    val screen = mutable.Map[Vec2, Tile]()

    @tailrec
    def execute(): Unit = {
      val output = for {
        x <- execution.continue()
        y <- execution.continue()
        tileNum <- execution.continue()
      } yield {
        val pos = Vec2(x.toInt, y.toInt)
        val tile = Tile(tileNum)

        print("\u001B[0;0H")
        println(mapToString(screen.view.mapValues(_.char)))

        screen.put(pos, tile)
      }
      output match {
        case Some(_) => execute()
        case None =>
      }
    }

    execute()

    screen.values.count(_ == Block())
  }
}
