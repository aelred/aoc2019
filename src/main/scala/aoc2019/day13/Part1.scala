package aoc2019.day13

import aoc2019.program.Program
import aoc2019.{Solution, Vec2, mapToString}

import scala.collection.mutable

object Part1 extends Solution[Program] {
  def solution: Int = {
    val program = line
    val screen = mutable.Map[Vec2, Tile]()

    program.start() { next =>
      val pos = Vec2(next().toInt, next().toInt)
      val tile = Tile(next())

      print("\u001B[0;0H")
      println(mapToString(screen.view.mapValues(_.char)))

      screen.put(pos, tile)
    }

    screen.values.count(_ == Block())
  }
}
