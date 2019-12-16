package aoc2019.day2

import aoc2019.Solution
import aoc2019.program.Program

object Part1 extends Solution[Program] {

  def solution: Long = line.withMemory(1 -> 12, 2 -> 2).executeAndReturnMemory(0)
}
