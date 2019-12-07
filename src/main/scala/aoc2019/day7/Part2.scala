package aoc2019.day7

import aoc2019.Solution
import aoc2019.program.Program

object Part2 extends Solution[Program] {

  def solution: Int = {
    val program = line
    val allPhaseSettings = (5 to 9).permutations

    val outputs = for {
      phaseSettings  <- allPhaseSettings

      _ = program.execute(0)

    } yield output

    outputs.max
  }
}
