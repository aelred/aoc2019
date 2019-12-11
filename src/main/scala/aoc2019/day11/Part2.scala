package aoc2019.day11

import aoc2019.Solution
import aoc2019.program.Program

import scala.collection.mutable

object Part2 extends Solution[Program] {

  def solution: String = {
    val hull = Robot.run(line)

    val xs = hull.keys.map(_.x)
    val ys = hull.keys.map(_.y)
    val left = xs.min
    val top = ys.min
    val width = xs.max - left + 1
    val height = ys.max - top + 1

    val table = mutable.Seq.fill(height, width)(' ')

    for ((pos, color) <- hull) {
      if (color == Robot.White) {
        table(pos.y - top)(pos.x - left) = '█'
      }
    }

    table.map(_.mkString).mkString("\n")
  }

}
