package aoc2019.day11

import aoc2019.Solution
import aoc2019.mapToString
import aoc2019.program.Program

object Part2 extends Solution[Program] {

  def solution: String = {
    val hull = Robot.run(line)

    mapToString(hull.view.mapValues {
      case Robot.White => '█'
      case Robot.Black => ' '
    })
  }
}
