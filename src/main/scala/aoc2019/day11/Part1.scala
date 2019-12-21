package aoc2019.day11

import aoc2019.Solution
import aoc2019.program.Program

object Part1 extends Solution[Program] {

  def solution: Int = {
    val hull = Robot.run(input)
    hull.keys.size
  }
}
